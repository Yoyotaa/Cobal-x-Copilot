import os
import shutil
import subprocess
from pathlib import Path
import pytest


pytestmark = pytest.mark.skipif(
    shutil.which("cobc") is None,
    reason="GnuCOBOL (cobc) not installed; skipping COBOL integration tests",
)


def compile_cobol_executable(tmp_path: Path) -> Path:
    repo_root = Path(__file__).resolve().parents[3]
    sources = [
        str(repo_root / "main.cob"),
        str(repo_root / "operations.cob"),
        str(repo_root / "data.cob"),
    ]
    exe_path = tmp_path / "mainprog"
    result = subprocess.run(
        ["cobc", "-x", "-o", str(exe_path), *sources],
        cwd=str(repo_root),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        raise RuntimeError(f"cobc compile failed: {result.stderr}\n{result.stdout}")
    return exe_path


def run_program(exe: Path, input_text: str) -> str:
    proc = subprocess.run(
        [str(exe)],
        input=input_text,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        check=False,
    )
    return proc.stdout


def test_cobol_view_balance_then_exit(tmp_path):
    exe = compile_cobol_executable(tmp_path)
    output = run_program(exe, "1\n4\n")
    assert "Account Management System" in output
    assert "Current balance:" in output
    assert "Exiting the program. Goodbye!" in output


def test_cobol_credit_then_view_then_exit(tmp_path):
    exe = compile_cobol_executable(tmp_path)
    output = run_program(exe, "2\n150\n1\n4\n")
    assert "Enter credit amount:" in output
    assert "Amount credited. New balance:" in output
    assert "Current balance:" in output


def test_cobol_debit_insufficient_then_exit(tmp_path):
    exe = compile_cobol_executable(tmp_path)
    output = run_program(exe, "3\n2000\n4\n")
    assert "Enter debit amount:" in output
    assert "Insufficient funds for this debit." in output
    assert "Exiting the program. Goodbye!" in output
