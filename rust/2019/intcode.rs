pub struct IntCode {
    mem: Vec<i64>,
    pc: usize,
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
    Add,
    Mul,
    Halt,
}

impl TryFrom<i64> for Instruction {
    type Error = anyhow::Error;
    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Instruction::Add),
            2 => Ok(Instruction::Mul),
            99 => Ok(Instruction::Halt),
            _ => Err(anyhow::Error::msg("invalid instruction id")),
        }
    }
}

impl IntCode {
    pub fn new(s: &[i64]) -> Self {
        let mut mem = s.to_vec();
        mem.resize(1000000, 0);
        Self { mem, pc: 0 }
    }

    pub fn set_mem(&mut self, pos: usize, val: i64) {
        self.mem[pos] = val;
    }

    pub fn get_mem(&self, pos: usize) -> i64 {
        self.mem[pos]
    }

    pub fn run(&mut self) -> anyhow::Result<()> {
        let Self { mem, pc } = self;
        let instruction = Instruction::try_from(mem[*pc])?;
        match instruction {
            Instruction::Add => {
                let op_1 = usize::try_from(mem[*pc + 1])?;
                let op_2 = usize::try_from(mem[*pc + 2])?;
                let op_3 = usize::try_from(mem[*pc + 3])?;
                mem[op_3] = mem[op_1] + mem[op_2];
                *pc += 4;
            }
            Instruction::Mul => {
                let op_1 = usize::try_from(mem[*pc + 1])?;
                let op_2 = usize::try_from(mem[*pc + 2])?;
                let op_3 = usize::try_from(mem[*pc + 3])?;
                mem[op_3] = mem[op_1] * mem[op_2];
                *pc += 4;
            }
            Instruction::Halt => return Ok(()),
        };
        self.run()
    }
}
