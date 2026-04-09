"""Normalize and copy an executed Jupyter notebook.

Strips execution metadata (execution counts, timestamps) that change between
runs and copies the normalized notebook to the target path. Used by CI to
produce a clean diff when deciding whether to open a PR.

Usage: python tests/normalize-notebook.py <source.ipynb> <target.ipynb>
"""
import json, sys


def normalize(nb: dict) -> None:
    for cell in nb.get('cells', []):
        cell['execution_count'] = None
        cell.get('metadata', {}).pop('execution', None)
        for out in cell.get('outputs', []):
            out.pop('execution_count', None)


if __name__ == '__main__':
    src, dst = sys.argv[1], sys.argv[2]
    with open(src) as f:
        nb = json.load(f)
    normalize(nb)
    with open(dst, 'w') as f:
        json.dump(nb, f, indent=1)
        f.write('\n')
