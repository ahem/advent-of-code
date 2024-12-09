use std::{
    io::{stdin, Read},
    str::FromStr,
};

use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
struct Free {
    size: i32,
}

#[derive(Debug, Clone, Copy)]
struct File {
    id: i32,
    size: i32,
}

#[derive(Debug, Clone, Copy)]
enum Cell {
    Free(Free),
    File(File),
}

#[derive(Debug, Clone)]
struct Disk {
    cells: Vec<Cell>,
}

impl FromStr for Disk {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let digits = s
            .trim()
            .chars()
            .map(|c| c.to_string().parse::<i32>())
            .collect::<Result<Vec<i32>, _>>()?;

        let cells = digits
            .iter()
            .enumerate()
            .map(|(idx, n)| {
                if idx % 2 == 0 {
                    Cell::File(File {
                        id: (idx / 2) as i32,
                        size: *n,
                    })
                } else {
                    Cell::Free(Free { size: *n })
                }
            })
            .collect_vec();
        Ok(Disk { cells })
    }
}

impl Disk {
    fn last_file(&self) -> Option<(usize, File)> {
        self.cells
            .iter()
            .enumerate()
            .rev()
            .find_map(|(idx, x)| match x {
                Cell::File(file) => Some((idx, *file)),
                _ => None,
            })
    }

    fn file_by_id(&self, id: i32) -> (usize, File) {
        self.cells
            .iter()
            .enumerate()
            .find_map(|(idx, cell)| match cell {
                Cell::File(file) if file.id == id => Some((
                    idx,
                    File {
                        id: file.id,
                        size: file.size,
                    },
                )),
                _ => None,
            })
            .unwrap()
    }

    fn first_free(&self, min_size: i32) -> Option<(usize, Free)> {
        self.cells
            .iter()
            .enumerate()
            .find_map(|(idx, cell)| match cell {
                Cell::Free(free) if free.size >= min_size => Some((idx, *free)),
                _ => None,
            })
    }

    fn digits(&self) -> impl Iterator<Item = Option<i32>> + '_ {
        self.cells.iter().flat_map(|cell| match cell {
            Cell::File(File { id, size }) => std::iter::repeat(Some(*id)).take(*size as _),
            Cell::Free(Free { size }) => std::iter::repeat(None).take(*size as _),
        })
    }

    fn show(&self) {
        let s = self
            .digits()
            .map(|x| match x {
                Some(n) => n.to_string(),
                _ => ".".to_string(),
            })
            .collect::<String>();
        println!("{s}");
    }

    fn compact(&mut self) {
        loop {
            // self.show();
            let (file_idx, file) = self.last_file().unwrap();
            let (free_idx, free) = self.first_free(0).unwrap();
            if free_idx > file_idx {
                break;
            }

            if file.size == free.size {
                self.cells[free_idx] = Cell::File(file);
                self.cells[file_idx] = Cell::Free(free);
            } else if file.size < free.size {
                self.cells[free_idx] = Cell::Free(Free {
                    size: free.size - file.size,
                });
                self.cells[file_idx] = Cell::Free(Free { size: file.size });
                self.cells.insert(free_idx, Cell::File(file));
            } else if file.size > free.size {
                self.cells[free_idx] = Cell::File(File {
                    id: file.id,
                    size: free.size,
                });
                self.cells[file_idx] = Cell::File(File {
                    id: file.id,
                    size: file.size - free.size,
                });
            }

            self.merge_free_if_needed(file_idx - 1);
        }
    }

    fn compact_defragged(&mut self) {
        let max_file_id = self.digits().filter_map(|x| x).max().unwrap();
        for id in (0..=max_file_id).rev() {
            // self.show();
            let (file_idx, file) = self.file_by_id(id);
            let (free_idx, free) = match self.first_free(file.size) {
                Some(o) => o,
                _ => continue,
            };
            if free_idx > file_idx {
                continue;
            }
            if file.size == free.size {
                self.cells[free_idx] = Cell::File(file);
                self.cells[file_idx] = Cell::Free(free);
            } else if file.size < free.size {
                self.cells[free_idx] = Cell::Free(Free {
                    size: free.size - file.size,
                });
                self.cells[file_idx] = Cell::Free(Free { size: file.size });
                self.cells.insert(free_idx, Cell::File(file));
            } else {
                unreachable!();
            }

            self.merge_free_if_needed(file_idx - 1);
        }
    }

    fn merge_free_if_needed(&mut self, idx: usize) {
        let a = match self.cells[idx] {
            Cell::Free(free) => Some(free),
            _ => None,
        };
        let b = match self.cells[idx + 1] {
            Cell::Free(free) => Some(free),
            _ => None,
        };

        if a.is_some() && b.is_some() {
            let size = a.unwrap().size + b.unwrap().size;
            self.cells.remove(idx);
            self.cells[idx] = Cell::Free(Free { size });
        }
    }

    fn checksum(&self) -> i64 {
        self.digits()
            .enumerate()
            .filter_map(|(idx, n)| match n {
                Some(n) => Some(idx as i64 * n as i64),
                None => None,
            })
            .sum()
    }
}

pub fn main() -> anyhow::Result<()> {
    let disk = {
        let mut s = String::new();
        stdin().read_to_string(&mut s)?;
        s.parse::<Disk>()?
    };

    {
        let mut disk = disk.clone();
        disk.compact();
        println!("part 1: {}", disk.checksum());
    }
    {
        let mut disk = disk.clone();
        disk.compact_defragged();
        println!("part 2: {}", disk.checksum());
    }

    Ok(())
}
