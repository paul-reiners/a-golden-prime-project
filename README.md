Directory Structure
===================

Based on ["Structure of a Haskell project"](http://www.haskell.org/haskellwiki/Structure_of_a_Haskell_project).

    a-golden-prime-project/ -- Root-dir
      src/                  -- For keeping the sourcecode
        Main.lhs            -- The main-module
        App/                -- Use hierarchical modules
          ...
          Win32/            -- For system dependent stuff
          Unix/
        cbits/              -- For C code to be linked to the haskell program
      testsuite/            -- Contains the testing stuff
        runtests.sh         -- Will run all tests
        tests/              -- For unit-testing and checking
          App/              -- Clone the module hierarchy, so that there is one 
                               testfile per sourcefile
        benchmarks/         -- For testing performance
      doc/                  -- Contains the manual, and other documentation
        examples/           -- Example inputs for the program
        dev/                -- Information for new developers about the project, 
                               and eg. related literature
      util/                 -- Auxiliary scripts for various tasks
      dist/                 -- Directory containing what end-users should get
        build/              -- Contains binary files, created by cabal
        doc/                -- The haddock documentation goes here, created by cabal
        resources/          -- Images, soundfiles and other non-source stuff
                               used by the program
      _DARCS/        
      README.md             -- Textfile with short introduction of the project
      INSTALL               -- Textfile describing how to build and install
      TODO                  -- Textfile describing things that ought to be done
      AUTHORS               -- Textfile containing info on who does and has done 
                               what in this project, and their contact info
      LICENSE               -- Textfile describing licensing terms for this project
      app.cabal             -- Project-description-file for cabal
      Setup.hs              -- Program for running cabal commands
