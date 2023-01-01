use std::collections::HashSet;

#[derive(Copy, Clone)]
enum VentDirection {
    Left,
    Right,
}

fn parse(input: &str) -> Vec<VentDirection> {
    input
        .chars()
        .map(|c| match c {
            '<' => VentDirection::Left,
            _ => VentDirection::Right,
        })
        .collect()
}
fn hobar(yoffset: usize) -> Vec<(i16, usize)> {
    vec![2, 3, 4, 5].into_iter().map(|x| (x, yoffset)).collect()
}
fn cross(yoffset: usize) -> Vec<(i16, usize)> {
    vec![
        (3, yoffset + 2),
        (2, yoffset + 1),
        (3, yoffset + 1),
        (4, yoffset + 1),
        (3, yoffset),
    ]
}
fn lbar(yoffset: usize) -> Vec<(i16, usize)> {
    vec![
        (2, yoffset),
        (3, yoffset),
        (4, yoffset),
        (4, yoffset + 1),
        (4, yoffset + 2),
    ]
}
fn vertbar(yoffset: usize) -> Vec<(i16, usize)> {
    vec![yoffset, yoffset + 1, yoffset + 2, yoffset + 3]
        .into_iter()
        .map(|y| (2, y))
        .collect()
}
fn square(yoffset: usize) -> Vec<(i16, usize)> {
    vec![
        (2, yoffset),
        (3, yoffset),
        (2, yoffset + 1),
        (3, yoffset + 1),
    ]
}
fn push(
    falling_rocks: &mut [(i16, usize)],
    stationary_rocks: &HashSet<(i16, usize)>,
    direction: VentDirection,
) {
    use VentDirection::*;
    let (dx, would_hit_wall) = match direction {
        Right => (1, Box::new(|x: &i16| *x == 6) as Box<dyn Fn(&i16) -> bool>),
        Left => (-1, Box::new(|x: &i16| *x == 0) as Box<dyn Fn(&i16) -> bool>),
    };
    let would_hit_stationary = {
        let falling_rocks_ys = falling_rocks
            .iter()
            .map(|(_x, y)| *y)
            .collect::<HashSet<_>>();
        let pertinent_stationaries: HashSet<_> = stationary_rocks
            .iter()
            .filter(|(_x, y)| falling_rocks_ys.contains(y))
            .collect();
        falling_rocks
            .iter()
            .any(|(x, y)| pertinent_stationaries.contains(&(x + dx, *y)))
    };
    if would_hit_stationary || falling_rocks.iter().any(|(x, _y)| would_hit_wall(x)) {
        return;
    }
    falling_rocks.iter_mut().for_each(|pos| pos.0 += dx)
}

fn simulate(
    fans: &[VentDirection],
    mut fan_index: usize,
    mut falling_rocks: Vec<(i16, usize)>,
    stationary_rocks: &mut HashSet<(i16, usize)>,
) -> usize {
    loop {
        push(
            &mut falling_rocks,
            &stationary_rocks,
            fans[fan_index % fans.len()],
        );
        let fall_state = fall(&mut falling_rocks, &stationary_rocks);
        if matches!(fall_state, Falling::Landed) {
            falling_rocks.into_iter().for_each(|pos| {
                stationary_rocks.insert(pos);
            });
            return fan_index + 1;
        }
        fan_index += 1;
    }
}

fn part1(input: &str) -> usize {
    let fans = parse(input);
    let mut corridor = HashSet::new();

    (0..2022).fold(0, |fan_index, shape_index| {
        let max_height = corridor
            .iter()
            .max_by_key(|(x, y)| y)
            .map(|(x, y)| *y)
            .unwrap_or(0);

        let shapes = vec![
            hobar(max_height + 4),
            cross(max_height + 4),
            lbar(max_height + 4),
            vertbar(max_height + 4),
            square(max_height + 4),
        ];
        let mut shape = shapes[shape_index % shapes.len()].clone();
        simulate(&fans, fan_index, shape, &mut corridor)
    });

    corridor.iter().max_by_key(|(x, y)| y).unwrap().1
}
enum Falling {
    StillFalling,
    Landed,
}

fn fall(falling_rocks: &mut [(i16, usize)], stationary_rocks: &HashSet<(i16, usize)>) -> Falling {
    if falling_rocks.iter().any(|(x, y)| *y == 1) {
        return Falling::Landed;
    }

    let mut next = falling_rocks.iter().map(|(x, y)| (*x, *y - 1));
    if next.any(|pos| stationary_rocks.contains(&pos)) {
        return Falling::Landed;
    }
    falling_rocks.iter_mut().for_each(|pos| pos.1 -= 1);
    return Falling::StillFalling;
}
fn main() {
    println!("Hello, world!");
}

#[test]
fn test_sample() {
    let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";
    assert_eq!(part1(input), 3068)
}
