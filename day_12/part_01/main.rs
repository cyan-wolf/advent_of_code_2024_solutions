use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

type Grid<T> = Vec<Vec<T>>;

struct Plot {
    perimeter: usize,
}

fn read_input() -> io::Result<Grid<char>> {
    let f = File::open("input.txt")?;

    let br = BufReader::new(f);

    let grid = br.lines()
        .map(|l| {
            l.unwrap().chars().collect::<Vec<char>>()
        })
        .collect();

    Ok(grid)
}

fn get_from_grid(grid: &Grid<char>, r: usize, c: usize) -> Option<char> {
    grid.get(r)?.get(c).cloned()
}

fn calc_area_perimeter(grid: Grid<char>) -> HashMap<char, Vec<Plot>> {
    let mut farm: HashMap<char, Vec<Plot>> = HashMap::new();

    for r in 0..grid.len() {
        for c in 0..grid[0].len() {
            let ch = get_from_grid(&grid, r, c).unwrap();

            let nbrs = [
                [r + 1, c],
                [r.overflowing_sub(1).0, c],
                [r, c + 1],
                [r, c.overflowing_sub(1).0],
            ];

            let same_nbrs = nbrs.into_iter()
                .filter_map(|[nr, nc]| get_from_grid(&grid, nr, nc))
                .filter(|nbr| nbr == &ch)
                .count();

            let perimeter = 4 - same_nbrs;

            let plot = Plot { perimeter };

            farm
                .entry(ch)
                .or_default()
                .push(plot);
        }
    }

    farm
}

fn calc_total_cost(farm: HashMap<char, Vec<Plot>>) -> usize {
    let mut cost = 0;

    for plots in farm.values() {
        let area = plots.len();
        let perimeter: usize = plots.iter()
            .map(|p| p.perimeter)
            .sum();

        cost += area * perimeter;
    }
    cost
}

fn main() -> io::Result<()> {
    let grid = read_input()?;
    let farm = calc_area_perimeter(grid);
    let cost = calc_total_cost(farm);

    println!("{cost}");

    Ok(())
}
