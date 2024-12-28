use std::cell::RefCell;
use std::rc::Rc;

use std::time::Instant;

thread_local! {
    static ARENA_INTERN: RefCell<Arena1> = RefCell::new(Arena1::new());
}

#[allow(dead_code)]
struct XXX {
    u: f64,
    a: f64,
    b: f64,
    c: f64,
    d: f64,
}

#[allow(unused)]
enum Val {
    None,
    N(AR1),
    X(XXX),
}

#[allow(dead_code)]
enum Val2 {
    None,
    N(Rc<(Val2, Val2)>),
    X(XXX),
}

impl Val {
    pub fn n(a: Val, b: Val) -> Self {
        Val::N(AR1::new(a, b))
    }
    pub fn x(a: f64) -> Self {
        let mut x = XXX::new();
        x.a = a;
        Val::X(x)
    }
}

impl Val2 {
    pub fn n(a: Val2, b: Val2) -> Self {
        Val2::N(Rc::new((a, b)))
    }
    pub fn x(a: f64) -> Self {
        let mut x = XXX::new();
        x.a = a;
        Val2::X(x)
    }
}

impl XXX {
    pub fn new() -> Self {
        Self { u: 0.0, a: 0.0, b: 0.0, c: 0.0, d: 0.0 }
    }
}

struct Arena1 {
    free: Vec<Rc<(Val, Val)>>,
}

impl Arena1 {
    pub fn new() -> Self {
        let mut free = vec![];
        free.resize_with(10, || Rc::new((Val::None, Val::None)));
        Self { free }
    }

    #[inline]
    pub fn alloc(&mut self) -> Rc<(Val, Val)> {
        if let Some(a) = self.free.pop() {
            a
        } else {
            Rc::new((Val::None, Val::None))
        }
    }

    #[inline]
    pub fn free(&mut self, rc: &mut Rc<(Val, Val)>) {
        *Rc::get_mut(rc).unwrap() = (Val::None, Val::None);
        self.free.push(rc.clone());
    }
}

#[derive(Clone)]
struct AR1 {
    cont: Rc<(Val, Val)>,
}

impl AR1 {
    pub fn new(a: Val, b: Val) -> Self {
        ARENA_INTERN.with(|x| {
            let mut rc = x.borrow_mut().alloc();
            *Rc::get_mut(&mut rc).unwrap() = (a, b);
            Self { cont: rc }
        })
    }
}

impl Drop for AR1 {
    fn drop(&mut self) {
        if Rc::strong_count(&self.cont) == 1 {
            ARENA_INTERN.with(|x| {
                x.borrow_mut().free(&mut self.cont);
            })
        }
    }
}

fn t1(n: usize) {
    let mut v = vec![];
    let now = Instant::now();
    for _ in 0..10000 {
        for _ in 0..n {
            v.push(Val::n(Val::x(21.32), Val::x(22.1)));
        }
        v.clear();
        for _ in 0..n {
            v.push(Val::n(Val::x(21.32), Val::x(22.1)));
        }
        v.clear();
    }
    println!("arena [{:5}]: {}", n, now.elapsed().as_millis());
}

fn t2(n: usize) {
    let mut v = vec![];
    let now = Instant::now();
    for _ in 0..10000 {
        for _ in 0..n {
            v.push(Val2::n(Val2::x(21.32), Val2::x(22.1)));
        }
        v.clear();
        for _ in 0..n {
            v.push(Val2::n(Val2::x(21.32), Val2::x(22.1)));
        }
        v.clear();
    }
    println!("alloc [{:5}]: {}", n, now.elapsed().as_millis());
}

fn main() {
    for t in 0..4 {
        println!("*** {}", t);
        t1(3000);
        t2(3000);
        t1(10);
        t2(10);
        t1(100);
        t2(100);
        t1(500);
        t2(500);
        t1(1000);
        t2(1000);
    }
}
