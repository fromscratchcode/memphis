#[cfg(test)]
// These tests exist to test that the memphis binary works properly in script mode and correctly
// responds to the MEMPHIS_ENGINE environment variable. For semantic tests, look in the specific
// engine or in crosscheck, which do not boot another process.
mod integration_tests {
    use std::{
        path::{Path, PathBuf},
        process::Command,
    };

    fn workspace_path(path: &str) -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("memphis-cli crate should live under the workspace root")
            .join(path)
    }

    fn run_output(binary: &str, script: &'static str, engine: Option<&str>) -> String {
        let mut command = Command::new(binary);

        if let Some(engine_name) = engine {
            command.env("MEMPHIS_ENGINE", engine_name);
        }

        let output = command
            .arg(workspace_path(script))
            .output()
            .expect("Failed to run test script");

        if !output.status.success() {
            panic!("Running script {} failed.", script);
        }

        String::from_utf8_lossy(&output.stdout).to_string()
    }

    fn test_script(script: &'static str, expected: &'static str, engine: Option<&str>) {
        let output = run_output(env!("CARGO_BIN_EXE_memphis"), script, engine);

        assert_eq!(
            output, expected,
            "Running script {} produced unexpected output.",
            script
        );
    }

    #[test]
    fn run_treewalk_script() {
        test_script(
            "fixtures/test.py",
            include_str!("../../fixtures/test.stdout"),
            None,
        );
    }

    #[test]
    fn run_bytecode_vm_script() {
        test_script(
            "fixtures/test_vm.py",
            include_str!("../../fixtures/test_vm.stdout"),
            Some("bytecode_vm"),
        );
    }
}
