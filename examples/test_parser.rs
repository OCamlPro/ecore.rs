ecore_rs::prelude! {}

pub const CONTENT: &str = include_str!("../rsc/ExampleEcore.ecore");

fn main() {
    simple_logger::SimpleLogger::new()
        .env()
        .without_timestamps()
        .init()
        .expect("failed to init logger /(-_-)\\");
    let ctx = Ctx::parse(CONTENT).unwrap_or_else(|err| {
        println!("an error occurred:");
        for line in err.to_string().lines() {
            println!("- {}", line)
        }
        panic!("run failed")
    });
    println!("|==| done \\(*o*)/");
    for line in ctx.to_pretty_string().lines() {
        println!("| {}", line);
    }
    println!("|==|")
}
