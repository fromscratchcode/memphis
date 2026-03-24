#[cfg(feature = "llvm_backend")]
#[cfg(test)]
mod llvm_backend_tests {
    use std::process::Command;

    /// Run memphis with the Engine::LlvmBackend engine and just confirm it doesn't fail.
    fn run_script(script: &'static str) {
        let output = Command::new("target/debug/memphis")
            .arg(script)
            .env("MEMPHIS_ENGINE", "llvm_backend")
            .output()
            .expect("Failed to run test script");

        if !output.status.success() {
            panic!("Running script {} failed.", script);
        }
    }

    fn ui_tests() -> Vec<&'static str> {
        vec!["examples/loop_perf.py"]
    }

    #[test]
    fn run_scripts() {
        for ui_test in ui_tests() {
            run_script(ui_test);
        }
    }
}
