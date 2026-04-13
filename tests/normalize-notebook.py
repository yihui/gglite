"""Normalize and copy an executed Jupyter notebook.

Strips execution metadata (execution counts, timestamps) that change between
runs and copies the normalized notebook to the target path. Also strips version
numbers from CDN resource URLs so that version bumps in upstream packages do
not trigger new PRs to update the .ipynb output.

Usage: python tests/normalize-notebook.py <source.ipynb> <target.ipynb>
"""
import json, re, sys


def strip_cdn_versions(s: str) -> str:
    """Remove version specifiers (e.g. @5, @v0.14.34) from CDN URLs in src/href attributes."""
    return re.sub(r'((src|href)="https://[^"]*?)@v?\d+(?:\.\d+)*', r'\1', s)


def normalize_value(v):
    if isinstance(v, str):
        return strip_cdn_versions(v)
    if isinstance(v, list):
        return [normalize_value(x) for x in v]
    return v


def normalize(nb: dict) -> None:
    for cell in nb.get('cells', []):
        cell['execution_count'] = None
        cell.get('metadata', {}).pop('execution', None)
        for out in cell.get('outputs', []):
            out.pop('execution_count', None)
            for key in ('text', 'data'):
                if key in out:
                    val = out[key]
                    if isinstance(val, list):
                        out[key] = [normalize_value(x) for x in val]
                    elif isinstance(val, dict):
                        out[key] = {k: normalize_value(v) for k, v in val.items()}


if __name__ == '__main__':
    src, dst = sys.argv[1], sys.argv[2]
    with open(src) as f:
        nb = json.load(f)
    normalize(nb)
    with open(dst, 'w') as f:
        json.dump(nb, f, indent=1)
        f.write('\n')
