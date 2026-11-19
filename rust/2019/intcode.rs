use anyhow::anyhow;
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};

pub struct IntCode {
    mem: Vec<i64>,
    pc: usize,
    input_sender: Option<SyncSender<i64>>,
    input: Option<Receiver<i64>>,
    output: Option<SyncSender<i64>>,
    output_receiver: Option<Receiver<i64>>,
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
    Add(Mode, Mode, Mode),
    Mul(Mode, Mode, Mode),
    Input(Mode),
    Output(Mode),
    JumpIfTrue(Mode, Mode),
    JumpIfFalse(Mode, Mode),
    LessThan(Mode, Mode, Mode),
    Equals(Mode, Mode, Mode),
    Halt,
}

impl TryFrom<i64> for Instruction {
    type Error = anyhow::Error;
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        let operands = [
            Mode::try_from((value / 100) % 10)?,
            Mode::try_from((value / 1000) % 10)?,
            Mode::try_from((value / 10000) % 10)?,
        ];

        match value % 100 {
            1 => Ok(Instruction::Add(operands[0], operands[1], operands[2])),
            2 => Ok(Instruction::Mul(operands[0], operands[1], operands[2])),
            3 => Ok(Instruction::Input(operands[0])),
            4 => Ok(Instruction::Output(operands[0])),
            5 => Ok(Instruction::JumpIfTrue(operands[0], operands[1])),
            6 => Ok(Instruction::JumpIfFalse(operands[0], operands[1])),
            7 => Ok(Instruction::LessThan(operands[0], operands[1], operands[2])),
            8 => Ok(Instruction::Equals(operands[0], operands[1], operands[2])),
            99 => Ok(Instruction::Halt),
            other => Err(anyhow!("invalid instruction id: {other}")),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Mode {
    Position,
    Immediate,
}

impl TryFrom<i64> for Mode {
    type Error = anyhow::Error;
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Mode::Position),
            1 => Ok(Mode::Immediate),
            other => Err(anyhow!("invalid parameter mode: {other}")),
        }
    }
}

impl IntCode {
    pub fn new(s: &[i64]) -> Self {
        let mut mem = s.to_vec();
        mem.resize(1000000, 0);
        let (input_sender, input) = sync_channel(100);
        let (output, output_receiver) = sync_channel(100);
        Self {
            mem,
            pc: 0,
            input_sender: Some(input_sender),
            input: Some(input),
            output: Some(output),
            output_receiver: Some(output_receiver),
        }
    }

    pub fn set_mem(&mut self, pos: usize, val: i64) {
        self.mem[pos] = val;
    }

    pub fn get_mem(&self, pos: usize) -> i64 {
        self.mem[pos]
    }

    pub fn send_input(&self, val: i64) -> anyhow::Result<()> {
        self.input_sender
            .as_ref()
            .ok_or(anyhow!("input channel lost"))?
            .send(val)?;
        Ok(())
    }

    pub fn read_output(&self) -> anyhow::Result<i64> {
        Ok(self
            .output_receiver
            .as_ref()
            .ok_or(anyhow!("output channel lost"))?
            .recv()?)
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
        while self.step()? {
            continue;
        }
        Ok(())
    }

    pub fn run_async(
        mut self,
    ) -> anyhow::Result<(
        SyncSender<i64>,
        Receiver<i64>,
        std::thread::JoinHandle<anyhow::Result<()>>,
    )> {
        let input = self
            .input_sender
            .take()
            .ok_or(anyhow!("lost input channel"))?;
        let output = self
            .output_receiver
            .take()
            .ok_or(anyhow!("lost output channel"))?;
        let join_handle = std::thread::spawn(move || self.run());
        Ok((input, output, join_handle))
    }

    fn step(&mut self) -> anyhow::Result<bool> {
        let pc = self.pc;
        let instruction = Instruction::try_from(self.mem[pc])?;
        match instruction {
            Instruction::Add(mode_1, mode_2, mode_3) => {
                let a = self.load_operand(mode_1, pc + 1)?;
                let b = self.load_operand(mode_2, pc + 2)?;
                let dst = self.load_dst_operand(mode_3, pc + 3)?;
                self.mem[dst] = a + b;
                self.pc += 4;
            }
            Instruction::Mul(mode_1, mode_2, mode_3) => {
                let a = self.load_operand(mode_1, pc + 1)?;
                let b = self.load_operand(mode_2, pc + 2)?;
                let dst = self.load_dst_operand(mode_3, pc + 3)?;
                self.mem[dst] = a * b;
                self.pc += 4;
            }
            Instruction::Input(mode) => {
                let dst = self.load_dst_operand(mode, pc + 1)?;
                let input_channel = self.input.as_ref().ok_or(anyhow!("input after halt"))?;
                self.mem[dst] = input_channel.recv()?;
                self.pc += 2;
            }
            Instruction::Output(mode) => {
                let src = self.load_operand(mode, pc + 1)?;
                let output_channel = self.output.as_ref().ok_or(anyhow!("output after halt"))?;
                output_channel.send(src)?;
                self.pc += 2;
            }
            Instruction::JumpIfTrue(mode_1, mode_2) => {
                let val = self.load_operand(mode_1, pc + 1)?;
                let target = usize::try_from(self.load_operand(mode_2, pc + 2)?)?;
                if val > 0 {
                    self.pc = target;
                } else {
                    self.pc += 3;
                }
            }
            Instruction::JumpIfFalse(mode_1, mode_2) => {
                let val = self.load_operand(mode_1, pc + 1)?;
                let target = usize::try_from(self.load_operand(mode_2, pc + 2)?)?;
                if val == 0 {
                    self.pc = target;
                } else {
                    self.pc += 3;
                }
            }
            Instruction::LessThan(mode_1, mode_2, mode_3) => {
                let a = self.load_operand(mode_1, pc + 1)?;
                let b = self.load_operand(mode_2, pc + 2)?;
                let dst = self.load_dst_operand(mode_3, pc + 3)?;
                self.mem[dst] = if a < b { 1 } else { 0 };
                self.pc += 4;
            }
            Instruction::Equals(mode_1, mode_2, mode_3) => {
                let a = self.load_operand(mode_1, pc + 1)?;
                let b = self.load_operand(mode_2, pc + 2)?;
                let dsdt = self.load_dst_operand(mode_3, pc + 3)?;
                self.mem[dsdt] = if a == b { 1 } else { 0 };
                self.pc += 4;
            }
            Instruction::Halt => {
                self.input = None;
                self.output = None;
                return Ok(false);
            }
        };
        Ok(true)
    }

    fn load_operand(&self, mode: Mode, addr: usize) -> anyhow::Result<i64> {
        match mode {
            Mode::Position => Ok(self.mem[usize::try_from(self.mem[addr])?]),
            Mode::Immediate => Ok(self.mem[addr]),
        }
    }

    fn load_dst_operand(&self, mode: Mode, addr: usize) -> anyhow::Result<usize> {
        match mode {
            Mode::Position => Ok(usize::try_from(self.mem[addr])?),
            Mode::Immediate => Err(anyhow!("illegal immediate mode for dst operand")),
        }
    }
}
