self: super: {
    defaultPythonEnv = super.buildEnv {
        name = "defaultPythonEnv";
        paths = [
            (self.python3.withPackages (
                ps: with ps; [
                    ipython

                    # Math stuff
                    numpy scipy matplotlib
                ]
                ))
            ];
        };
    }
