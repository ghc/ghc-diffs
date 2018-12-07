module Rules.Libffi (libffiRules, libffiDependencies) where

import Hadrian.Utilities

import Packages
import Settings.Builders.Common
import Target
import Utilities

{-
Note [Hadrian: install libffi hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are 2 important steps in handling libffi's .a and .so files:

  1. libffi's .a and .so files are copied from the libffi build dir to the rts
  build dir. This is because libffi is ultimately bundled with the rts package.
  Relevant code is in libffiRules.
  2. The rts is "installed" via the hadrian/src/Hadrian/Haskell/Cabal/Parse.hs:
  copyPackage action. This uses the "cabal copy" command which attempt to copy
  the relevant .a and .so files as well as many other files to the install dir.

If hadrian eventually tries to link the ghc binary whith the previous stage's
ghc, that ghc sees rts as a dependancyt. Rts has libCffi as an
"extra-bundled-library" and hence links with the -lffi option. The linker then
requires that the install dir contain "libffi.so", "libffi.so.7", and
"libffi.so.7.1.0". This, unfortunatelly is at odds with cabal's behavior which
expects an .a and .so file per way with the name
libCffi-<way_suffix>-ghc<ghc version>.(a|so). This only seems to be a problem
when dynamically linking (hadrian manages to link ghc statically using only the
cabal convention for the .a files).

Currently, to make both cabal and ghc happy in the dynamic case, we implement
the cabal convention but also have a hack to copy the .so files with the ghc
convention. The .so files generated when building libffi already have the
correct naming convention so we simply copy all files matching "libffi.so*".
This logic is implemented in both step 1 and 2 listed above.

This should eventually be revisited and the hack removed. A better solution
would be to align the naming conventions. It should be possible to change
cabal's behavior to match ghc's. Luckily "extra-bundled-library" seems to be
used exclusively by the rts package (as revield by a google/github search), so
breaking user code is of little concern here.

See comment 14 of issue #15837 for original comments on this.
-}

libffiDependencies :: [FilePath]
libffiDependencies = ["ffi.h", "ffitarget.h"]

libffiLibraryDistDir :: FilePath
libffiLibraryDistDir = "inst/lib"

libffiDynamicLibraryFile :: FilePath
libffiDynamicLibraryFile = "libffi.so"

libffiStaticLibrary :: FilePath
libffiStaticLibrary = libffiLibraryDistDir -/- "libffi.a"

libffiDynamicLibrary :: FilePath
libffiDynamicLibrary = libffiLibraryDistDir -/- libffiDynamicLibraryFile

libffiLibraries :: [FilePath]
libffiLibraries = [libffiStaticLibrary, libffiDynamicLibrary]

-- | Given a way, this return the path to the .a or .so libffi library file.
-- after building libffi, the .a and .so files will be copied to these paths.
-- These paths will be under the rts build directory as libffi is bundled with
-- the rts package.
rtsLibffiLibrary :: Way -> Action FilePath
rtsLibffiLibrary way = do
    name    <- libffiLibraryName
    suf     <- libsuf way
    rtsPath <- rtsBuildPath
    return $ rtsPath -/- "lib" ++ name ++ suf

fixLibffiMakefile :: FilePath -> String -> String
fixLibffiMakefile top =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" ("$(subst ../install-sh," ++ top ++ "/install-sh,@INSTALL@)")

-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    cFlags  <- interpretInContext libffiContext $ mconcat
               [ cArgs
               , getStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext libffiContext ldArgs
    sequence [ builderEnvironment "CC" $ Cc CompileC Stage1
             , builderEnvironment "CXX" $ Cc CompileC Stage1
             , builderEnvironment "LD" (Ld Stage1)
             , builderEnvironment "AR" (Ar Unpack Stage1)
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

libffiRules :: Rules ()
libffiRules = do
    root <- buildRootRules
    fmap ((root <//> "rts/build") -/-) libffiDependencies &%> \_ -> do
        libffiPath <- libffiBuildPath
        need (fmap (libffiPath -/-) libffiLibraries)

    -- we set a higher priority because this overlaps
    -- with the static lib rule from Rules.Library.libraryRules.

    priority 2.0 $ fmap (root <//>) libffiLibraries &%> \_ -> do
        useSystemFfi <- flag UseSystemFfi
        rtsPath      <- rtsBuildPath
        if useSystemFfi
        then do
            ffiIncludeDir <- setting FfiIncludeDir
            putBuild "| System supplied FFI library will be used"
            forM_ ["ffi.h", "ffitarget.h"] $ \file ->
                copyFile (ffiIncludeDir -/- file) (rtsPath -/- file)
            putSuccess "| Successfully copied system FFI library header files"
        else do
            libffiPath <- libffiBuildPath
            build $ target libffiContext (Make libffiPath) [] []

            hs <- getDirectoryFiles "" [libffiPath -/- "inst/include/*"]
            forM_ hs $ \header ->
                copyFile header (rtsPath -/- takeFileName header)

            ways <- nubOrd <$> interpretInContext
                                    libffiContext
                                    (getLibraryWays <> getRtsWays)

            -- Install static libraries.
            -- See Note [Hadrian: install libffi hack].
            forM_ ways $ \way -> do
                rtsLib <- rtsLibffiLibrary way
                let libffiLibrary = if Dynamic `wayUnit` way
                                        then libffiDynamicLibrary
                                        else libffiStaticLibrary
                copyFileUntracked (libffiPath -/- libffiLibrary) rtsLib

            -- Install dynamic libraries.
            -- See Note [Hadrian: install libffi hack].
            when (any (wayUnit Dynamic) ways) $ do
                let libffiLibraryDistPath = libffiPath -/- libffiLibraryDistDir
                soFiles <- getDirectoryFiles
                                libffiLibraryDistPath
                                ["libffi.so*"]
                rtsPath <- rtsBuildPath
                forM_ soFiles $ \soFile -> copyFileUntracked
                    (libffiLibraryDistPath -/- soFile)
                    (rtsPath -/- soFile)

            putSuccess "| Successfully built custom library 'libffi'"

    root <//> "libffi/build/Makefile.in" %> \mkIn -> do
        libffiPath <- libffiBuildPath
        removeDirectory libffiPath
        tarball <- unifyPath . fromSingleton "Exactly one LibFFI tarball is expected"
               <$> getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]

        need [tarball]
        -- Go from 'libffi-3.99999+git20171002+77e130c.tar.gz' to 'libffi-3.99999'
        let libname = takeWhile (/= '+') $ takeFileName tarball

        root <- buildRoot
        removeDirectory (root -/- libname)
        -- TODO: Simplify.
        actionFinally (do
            build $ target libffiContext (Tar Extract) [tarball] [root]
            moveDirectory (root -/- libname) libffiPath) $
                removeFiles root [libname <//> "*"]

        top <- topDirectory
        fixFile mkIn (fixLibffiMakefile top)

    -- TODO: Get rid of hard-coded @libffi@.
    root <//> "libffi/build/Makefile" %> \mk -> do
        need [mk <.> "in"]
        libffiPath <- libffiBuildPath
        forM_ ["config.guess", "config.sub"] $ \file ->
            copyFile file (libffiPath -/- file)

        env <- configureEnvironment
        buildWithCmdOptions env $
            target libffiContext (Configure libffiPath) [mk <.> "in"] [mk]
