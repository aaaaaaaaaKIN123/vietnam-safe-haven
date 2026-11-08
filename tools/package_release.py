"""Create a source-only release from an explicit allowlist, with SHA-256 manifest."""
from __future__ import annotations

import hashlib
import json
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile

ROOT = Path(__file__).resolve().parents[1]
TOP_LEVEL = [
    "README.md", "VERSION", "requirements.txt", "requirements-research.txt",
    "AGENT_PROJECT_GUIDE.md", ".gitignore", ".gitattributes", ".cbmignore",
]
SCOPES = {
    "src": {".py", ".R", ".yaml", ".md"},
    "tests": {".py"}, "tools": {".py", ".R"},
    "docs": {".md"}, ".github": {".yml", ".md"},
    "data/scripts": {".py"},
}


def release_files() -> list[Path]:
    files = [ROOT / name for name in TOP_LEVEL]
    files.append(ROOT / "data/AGENT_DATA_GUIDE.md")
    for folder, suffixes in SCOPES.items():
        for path in (ROOT / folder).rglob("*"):
            if "WS time series" in path.parts or "__pycache__" in path.parts:
                continue
            if path.is_file() and path.suffix in suffixes:
                if path.is_symlink() or not path.resolve().is_relative_to(ROOT.resolve()):
                    raise ValueError(f"Release path escapes project: {path}")
                files.append(path)
    return sorted(set(files))


def main() -> None:
    version = (ROOT / "VERSION").read_text(encoding="utf-8").strip()
    destination = ROOT / "dist" / f"vietnam-safe-haven-{version}-source.zip"
    destination.parent.mkdir(exist_ok=True)
    files = release_files()
    manifest = {
        "version": version, "kind": "source-only; no market data or fitted outputs",
        "sha256": {p.relative_to(ROOT).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest() for p in files},
    }
    with ZipFile(destination, "w", ZIP_DEFLATED) as archive:
        for path in files:
            archive.write(path, path.relative_to(ROOT).as_posix())
        archive.writestr("RELEASE_MANIFEST.json", json.dumps(manifest, indent=2))
    checksum = hashlib.sha256(destination.read_bytes()).hexdigest()
    destination.with_suffix(".zip.sha256").write_text(f"{checksum}  {destination.name}\n", encoding="utf-8")
    print(json.dumps({"archive": str(destination), "files": len(files), "sha256": checksum}, indent=2))


if __name__ == "__main__":
    main()
