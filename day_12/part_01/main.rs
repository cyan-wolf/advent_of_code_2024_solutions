use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

struct Plot {
    perimeter: usize,
}

type Grid = Vec<Vec<char>>;
type Region = Vec<Plot>;

fn read_input() -> io::Result<Grid> {
    let f = File::open("input.txt")?;

    let br = BufReader::new(f);

    let grid = br.lines()
        .map(|l| {
            l.unwrap().chars().collect::<Vec<char>>()
        })
        .collect();

    Ok(grid)
}

fn get_from_grid(grid: &Grid, r: usize, c: usize) -> Option<char> {
    grid.get(r)?.get(c).cloned()
}

fn pick_unproc_pos(unproc_postions: &mut HashSet<(usize, usize)>) -> (usize, usize) {
    let pos = unproc_postions.iter().next().copied().unwrap();
    pos
}

fn find_neighbors_in_same_region(
    pos: (usize, usize), 
    grid: &Grid, 
    unproc_postions: &mut HashSet<(usize, usize)>, 
    region: &mut Region,
) {
    // Process the current (given) position.
    let (r, c) = pos;
    unproc_postions.remove(&pos);

    // Get the character at the current position.
    let curr_ch = get_from_grid(&grid, r, c).unwrap();

    let nbrs = [
        (r + 1, c),
        (r.overflowing_sub(1).0, c),
        (r, c + 1),
        (r, c.overflowing_sub(1).0),
    ];

    // Manually count immediate neighbors to get the perimeter.
    let same_nbrs = nbrs.into_iter()
        .filter_map(|(nr, nc)| get_from_grid(&grid, nr, nc))
        .filter(|nbr| nbr == &curr_ch)
        .count();

    let perimeter = 4 - same_nbrs;

    // Add the plot to the region.
    region.push(Plot { perimeter });

    // Traverse the neighbors if they have the same character.
    for nbr_pos in nbrs {
        // Continue if position has not been processed.
        if unproc_postions.contains(&nbr_pos) {
            // Continue if position is in bounds.
            if let Some(nbr_ch) = get_from_grid(grid, nbr_pos.0, nbr_pos.1) {
                if curr_ch == nbr_ch {
                    find_neighbors_in_same_region(nbr_pos, grid, unproc_postions, region);
                }
            }
        }
    }

}

fn partition_regions(grid: Grid) -> Vec<Region> {
    let mut unproc_positions = HashSet::new();

    for r in 0..grid.len() {
        for c in 0..grid[0].len() {
            unproc_positions.insert((r, c));
        }
    }

    let mut regions = vec![];

    while !unproc_positions.is_empty() {
        let mut region = vec![];
        let pos = pick_unproc_pos(&mut unproc_positions);

        find_neighbors_in_same_region(pos, &grid, &mut unproc_positions, &mut region);

        regions.push(region);
    }
    regions
}

// Calculate the cost of regions.
fn calc_regions_cost(regions: Vec<Region>) -> usize {
    let mut cost = 0;

    for region in regions {
        let area = region.len();
        let perimeter: usize = region.iter().map(|p| p.perimeter).sum();
        cost += area * perimeter;
    }
    cost
}

fn main() -> io::Result<()> {
    let grid = read_input()?;
    let regions = partition_regions(grid);
    let cost = calc_regions_cost(regions);

    println!("{cost}");

    Ok(())
}
