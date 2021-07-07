#! /usr/bin/env python
# coding: utf-8

# pylint: disable=all

"""从aspell默认字典中筛选出小范围字典."""


import os
import subprocess


def _remove_duplicates(samples, suffix):
    new_samples = set()
    for sample in sorted(samples, key=len):
        subsample = sample[:-len(suffix)]
        if (not sample.endswith(suffix)) or (subsample not in new_samples):
            new_samples.add(sample)
    return list(new_samples)

cmd = "aspell -d en dump master | aspell -l en expand"
output = subprocess.run(cmd, shell=True, check=True, capture_output=True)
words = output.stdout.decode().split()
print("total words:        ", len(words))

words = list(filter(lambda x: "'" not in x, words))
print("filtered owner:      ", len(words))
words = list(filter(lambda x: not x[0].isupper(), words))
print("filtered upper:      ", len(words))

words = _remove_duplicates(words, "s")
print("filtered suffix_s:   ", len(words))
words = _remove_duplicates(words, "es")
print("filtered suffix_es:  ", len(words))
words = _remove_duplicates(words, "ly")
print("filtered suffix_ly:  ", len(words))
words = _remove_duplicates(words, "d")
print("filtered suffix_d:   ", len(words))
words = _remove_duplicates(words, "ed")
print("filtered suffix_ed:  ", len(words))
words = _remove_duplicates(words, "ing")
print("filtered suffix_ing: ", len(words))
words = _remove_duplicates(words, "er")
print("filtered suffix_er:  ", len(words))

words = list(filter(lambda x: len(x) > 4, words))
print("filtered length:     ", len(words))

with open("aspell.txt", "w") as dstfile:
    dstfile.write("\n".join(sorted(words, key=str.lower)) + "\n")
