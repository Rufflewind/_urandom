// cargo-deps: maze="^0.1.2"

// A demonstration of the A* algorithm in Rust.

extern crate maze;

macro_rules! impl_ord_supertraits {
    ( $name:tt, $( $params:tt ),* ) => {
        impl< $( $params ),* > PartialEq for $name < $( $params ),* > {
            fn eq(&self, other: &$name < $( $params ),* >) -> bool {
                self.partial_cmp(other) == Some(::std::cmp::Ordering::Equal)
            }
        }
        impl< $( $params ),* > Eq for $name < $( $params ),* > {}
        impl< $( $params ),* > PartialOrd for $name < $( $params ),* > {
            fn partial_cmp(&self, other: &$name < $( $params ),* >)
                           -> Option<::std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
    }
}

/// A* path-finding algorithm.
pub fn a_star<Node, Heuristic, Neighbors>(
    start: Node,
    goal: Node,
    mut neighbors: Neighbors,
    mut heuristic: Heuristic,
) -> Option<Vec<Node>>
    where Node: std::hash::Hash + Eq + Clone,
          Neighbors: FnMut(&Node, &mut Vec<(f64, Node)>),
          Heuristic: FnMut(&Node) -> f64,
{
    use std::collections::{BinaryHeap, HashMap, HashSet};
    use std::collections::hash_map::Entry;

    struct OpenNode<T>(f64, T);
    impl_ord_supertraits!(OpenNode, T);
    impl<T> Ord for OpenNode<T> {
        fn cmp(&self, other: &OpenNode<T>) -> std::cmp::Ordering {
            self.0.partial_cmp(&other.0).unwrap().reverse()
        }
    }
    let mut cost = HashMap::new();
    let mut came_from = HashMap::<Node, Node>::new();
    let mut closed_set = HashSet::new();
    let mut open_set = BinaryHeap::new();
    let mut neighbors_buf = Vec::new();
    cost.insert(start.clone(), 0.0);
    open_set.push(OpenNode(heuristic(&start), start));
    while let Some(OpenNode(_, current)) = open_set.pop() {
        if &current == &goal {
            let mut path = Vec::new();
            path.push(current);
            while let Some(c) = came_from.get(path.last().unwrap()) {
                path.push(c.clone());
            }
            path.reverse();
            return Some(path);
        }
        closed_set.insert(current.clone());
        let current_cost = cost.get(&current).unwrap().clone();
        neighbors(&current, &mut neighbors_buf);
        for &(dist, ref neighbor) in &neighbors_buf {
            if closed_set.contains(&neighbor) {
                continue;
            }
            let new_cost = current_cost + dist;
            match cost.entry(neighbor.clone()) {
                Entry::Occupied(ref e) if &new_cost >= e.get() => continue,
                e => e,
            }.or_insert(new_cost);
            came_from.insert(neighbor.clone(), current.clone());
            let est_cost = new_cost + heuristic(&neighbor);
            open_set.push(OpenNode(est_cost, neighbor.clone()));
        }
    }
    None
}

fn main() {
    use std::collections::HashSet;

    const DIM: usize = 39;
    const FPS: u32 = 60;
    const EMPTY: &'static str = "  ";
    const WALL: &'static str = "\x1b[41m  \x1b[0m";
    const PATH: &'static str = "\x1b[42m  \x1b[0m";
    const TRY: &'static str = "\x1b[44m  \x1b[0m";

    fn render_cells(cells: &[&str], dim: usize) {
        print!("\x1b[1;1H");
        for y in 0 .. dim {
            for x in 0 .. dim {
                print!("{}", cells[x + y * dim]);
            }
            print!("\n");
        }
    }

    assert!(DIM % 2 == 1);
    let mut wall = HashSet::new();
    let mut grid = maze::types::grid::Grid::<maze::types::cell::BaseCell>::new(
        DIM / 2 + 1, DIM / 2 + 1);
    grid.generate_aldous_broder();
    for y in 0 .. DIM {
        for x in 0 .. DIM {
            if x % 2 == 1 && y % 2 == 0 {
                if !grid.is_linked_indices(x / 2, y / 2, x / 2 + 1, y / 2) {
                    wall.insert((x, y));
                }
                if y > 0 && (wall.contains(&(x, y))
                             || wall.contains(&(x, y - 2))
                             || wall.contains(&(x - 1, y - 1))
                             || wall.contains(&(x + 1, y - 1))) {
                    wall.insert((x, y - 1));
                }
            } else if x % 2 == 0 && y % 2 == 1
                && !grid.is_linked_indices(x / 2, y / 2, x / 2, y / 2 + 1)
            {
                wall.insert((x, y));
            }
        }
    }

    print!("\x1b[2J"); // clear screen
    let mut cells = vec![EMPTY; DIM * DIM];
    for &(x, y) in &wall {
        cells[x + y * DIM] = WALL;
    }
    let goal = (DIM - 1, DIM - 1);
    let result = a_star(
        (0, 0),
        goal,
        |&(x, y), neighbors| {
            cells[x + y * DIM] = TRY;
            render_cells(&cells, DIM);
            let ten_millis = std::time::Duration::from_secs(1) / FPS;
            std::thread::sleep(ten_millis);
            neighbors.clear();
            neighbors.extend([
                (1.0, (x + 1, y)),
                (1.0, (x, y + 1)),
                (1.0, ((x as isize - 1) as usize, y)),
                (1.0, (x, (y as isize - 1) as usize)),
            ].iter().cloned().filter(|&(_, (x, y))| {
                x < DIM && y < DIM &&
                !wall.contains(&(x, y))
            }));
        },
        |&(x, y)| {
            let dx = x as f64 - goal.0 as f64;
            let dy = y as f64 - goal.1 as f64;
            dx * dx + dy * dy
        },
    );
    if let Some(r) = result {
        for &(x, y) in &r {
            cells[x + y * DIM] = PATH;
        }
    }
    render_cells(&cells, DIM);
}
