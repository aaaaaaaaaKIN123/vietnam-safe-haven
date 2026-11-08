"""Check archive integrity, source hashes and the release's data boundary."""
import hashlib
import json
from pathlib import Path
from zipfile import ZipFile

ROOT = Path(__file__).resolve().parents[1]


def main():
    version = (ROOT / "VERSION").read_text().strip()
    path = ROOT / "dist" / f"vietnam-safe-haven-{version}-source.zip"
    expected = path.with_suffix(".zip.sha256").read_text().split()[0]
    assert hashlib.sha256(path.read_bytes()).hexdigest() == expected, "Archive checksum mismatch"
    with ZipFile(path) as archive:
        assert archive.testzip() is None, "Corrupt archive member"
        names = set(archive.namelist())
        manifest = json.loads(archive.read("RELEASE_MANIFEST.json"))
        assert names == set(manifest["sha256"]) | {"RELEASE_MANIFEST.json"}, "Unmanifested files"
        for name, digest in manifest["sha256"].items():
            data = archive.read(name)
            assert hashlib.sha256(data).hexdigest() == digest, f"Hash mismatch: {name}"
            data.decode("utf-8-sig")
            assert name == Path(name).as_posix() and ".." not in Path(name).parts
            assert not name.startswith(("results/", "paper/", "ML/", "archive/", ".venv", "data/00_raw/", "data/02_model_input/")), name
            assert Path(name).suffix not in {".csv", ".xlsx", ".pdf", ".docx", ".pyc"}, name
    print(f"Verified {len(manifest['sha256'])} UTF-8 source files; hashes and data boundary passed.")


if __name__ == "__main__":
    main()
