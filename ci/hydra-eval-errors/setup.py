import setuptools

setuptools.setup(
    name="hydra-eval-errors",
    version="0.0.0",
    author="Samuel Leathers",
    author_email="samuel.leathers@iohk.io",
    description="hydra-eval-errors",
    long_description="hydra-eval-errors",
    url="https://github.com/input-output-hk/iohk-nix",
    packages=setuptools.find_packages(),
    scripts=['bin/hydra-eval-errors.py'],
    #entry_points={
    #    "console_scripts": [
    #        "hydra-eval-errors = hydra-eval-errors.script:main",
    #    ]
    #},
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache License",
        "Operating System :: OS Independent",
    ],
)
