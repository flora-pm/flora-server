module Flora.PackageFixtures where

import Data.Maybe
import Distribution.Parsec
import Distribution.SPDX
import Optics.Core

import Flora.Model.Package
import Flora.Model.Package.Types
import Flora.Model.Release
import Flora.Model.Requirement
import Flora.Model.User
import Flora.UserFixtures

-- ghc-prim

ghcPrim :: Package
ghcPrim =
  let packageId = PackageId (read "7a1ae448-3fc0-11ec-a038-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "ghc-prim"
      synopsis = "This package contains the primitive types and operations supplied by GHC."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://gitlab.haskell.org/ghc/ghc.git"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/ghc-prim/docs"
        , bugTracker = Just "https://gitlab.haskell.org/ghc/ghc/issues"
        }
   in Package{..}

ghcPrimRelease :: Release
ghcPrimRelease =
  let releaseId = ReleaseId $ read "3da0ae52-3fc1-11ec-ab06-5405db82c3cd"
      packageId = ghcPrim ^. #packageId
      version   = fromJust $ simpleParsec "0.8.0"
      archiveChecksum = "7a886c0629a6ec5f6edd07bf035192ce686dacc826fce70d06be2bead8480942"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

-- ghc-bignum

ghcBignum :: Package
ghcBignum =
  let packageId = PackageId (read "b845b706-3fc1-11ec-bf77-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "ghc-bignum"
      synopsis = "This package provides the low-level implementation of the standard BigNat, Natural and Integer types."
      ownerId = user2 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://gitlab.haskell.org/ghc/ghc.git"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/ghc-bignum/docs"
        , bugTracker = Just "https://gitlab.haskell.org/ghc/ghc/issues"
        }
   in Package{..}

ghcBignumRelease :: Release
ghcBignumRelease =
  let releaseId = ReleaseId $ read "6b934ab8-3fda-11ec-84dd-5405db82c3cd"
      packageId = ghcBignum ^. #packageId
      version   = fromJust $ simpleParsec "1.2"
      archiveChecksum = "6edc92e73f8cf890239acf3416ca7bcd34582f3ae2f46da06f07f69da5b57d21"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

ghcBignumDepOnGhcPrim :: Requirement
ghcBignumDepOnGhcPrim =
  let requirementId = RequirementId $ read "49b9b362-3fc8-11ec-9e41-5405db82c3cd"
      releaseId = ghcBignumRelease ^. #releaseId
      packageId = ghcPrim ^. #packageId
      requirement = ">=0.5.1.0 && <0.9"
   in Requirement{..}

-- base

base :: Package
base =
  let packageId = PackageId (read "88bda3f4-3fc6-11ec-bb71-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "base"
      synopsis = "This package contains the Standard Haskell Prelude and its support libraries, and a large collection of useful libraries ranging from data structures to parsing combinators and debugging utilities."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://gitlab.haskell.org/ghc/ghc.git"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/base/docs"
        , bugTracker = Just "https://gitlab.haskell.org/ghc/ghc/issues"
        }
   in Package{..}

baseRelease :: Release
baseRelease =
  let releaseId = ReleaseId $ read "558568cc-3fc2-11ec-985a-5405db82c3cd"
      packageId = base ^. #packageId
      version   = fromJust $ simpleParsec "4.16.0.0"
      archiveChecksum = "0920f977d4de4b325a160c15bb7fa50d8792530d44b66ea040cdc369601888c2"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

baseDepOnGhcPrim :: Requirement
baseDepOnGhcPrim =
  let requirementId = RequirementId $ read "11560fc0-3fc8-11ec-befa-5405db82c3cd"
      releaseId = baseRelease ^. #releaseId
      packageId = ghcPrim ^. #packageId
      requirement = ">=0.5.1.0 && <0.9"
   in Requirement{..}

baseDepOnGhcBignum :: Requirement
baseDepOnGhcBignum =
  let requirementId = RequirementId $ read "22839a88-3fc8-11ec-a22c-5405db82c3cd"
      releaseId = baseRelease ^. #releaseId
      packageId = ghcBignum ^. #packageId
      requirement = ">=1.0 && <2.0"
   in Requirement{..}

-- array

array :: Package
array =
  let packageId = PackageId (read "1bf55170-44c3-11ec-a2fd-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "array"
      synopsis = "In addition to providing the Data.Array module as specified in the Haskell 2010 Language Report, this package also defines the classes IArray of immutable arrays and MArray of arrays mutable within appropriate monads, as well as some instances of these classes."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "http://gitlab.haskell.org/ghc/packages/array.git"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/array/docs"
        , bugTracker = Just "https://gitlab.haskell.org/ghc/packages/array/issues"
        }
   in Package{..}

arrayRelease :: Release
arrayRelease =
  let releaseId = ReleaseId $ read "d095bad4-44c3-11ec-be1a-5405db82c3cd"
      packageId = array ^. #packageId
      version   = fromJust $ simpleParsec "0.5.4.0"
      archiveChecksum = "db21e0d842cc5d3bb89747114f5f1087fb71d9e16b03298614fab255074fc8cf"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

arrayDepOnBase :: Requirement
arrayDepOnBase =
  let requirementId = RequirementId $ read "07c1f7c6-44c3-11ec-bd48-5405db82c3cd"
      releaseId = arrayRelease ^. #releaseId
      packageId = base ^. #packageId
      requirement = ">=4.9 && <4.14"
   in Requirement{..}

-- deepseq

deepseq :: Package
deepseq =
  let packageId = PackageId (read "ea203e42-4a14-11ec-bd97-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "deepseq"
      synopsis = "This package provides methods for fully evaluating data structures (\"deep evaluation\")."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = " https://github.com/haskell/deepseq"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/deepseq/docs"
        , bugTracker = Just " https://github.com/haskell/deepseq/issues"
        }
   in Package{..}

deepseqRelease :: Release
deepseqRelease =
  let releaseId = ReleaseId $ read "3bb79f48-4a15-11ec-a991-5405db82c3cd"
      packageId = deepseq ^. #packageId
      version   = fromJust $ simpleParsec "1.4.6.1"
      archiveChecksum = "a8dbd24d4b622ea1bf278795d6869923519ed612e5b25f0ee6cabb6730957fc2"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

deepseqDepOnBase :: Requirement
deepseqDepOnBase =
  let requirementId = RequirementId $ read "52809324-4a15-11ec-a687-5405db82c3cd"
      releaseId = deepseqRelease ^. #releaseId
      packageId = base ^. #packageId
      requirement = ">=4.5 && <4.17"
   in Requirement{..}

deepseqDepOnArray :: Requirement
deepseqDepOnArray =
  let requirementId = RequirementId $ read "5ef583a8-4a15-11ec-8e68-5405db82c3cd"
      releaseId = deepseqRelease ^. #releaseId
      packageId = array ^. #packageId
      requirement = ">=0.4 && <0.6"
   in Requirement{..}

deepseqDepOnGhcPrim :: Requirement
deepseqDepOnGhcPrim =
  let requirementId = RequirementId $ read "7a602d1e-4a15-11ec-8ad7-5405db82c3cd"
      releaseId = deepseqRelease ^. #releaseId
      packageId = ghcPrim ^. #packageId
      requirement = ">=0.4 && <0.6"
   in Requirement{..}

-- containers

containers :: Package
containers =
  let packageId = PackageId (read "d9065730-4a15-11ec-873b-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "containers"
      synopsis = "This package contains efficient general-purpose implementations of various immutable container types including sets, maps, sequences, trees, and graphs."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://github.com/haskell/containers"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/containers/docs"
        , bugTracker = Just "https://github.com/haskell/containers/issues"
        }
   in Package{..}

containersRelease :: Release
containersRelease =
  let releaseId = ReleaseId $ read "08eb3286-4a16-11ec-b35f-5405db82c3cd"
      packageId = containers ^. #packageId
      version   = fromJust $ simpleParsec "0.6.5.1"
      archiveChecksum = "301a15c48c6e0466ce19ba1489b7178a2f632795b751724a8392bfa3e843ea45"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

containersDepOnBase :: Requirement
containersDepOnBase =
  let requirementId = RequirementId $ read "1944e730-4a16-11ec-a12e-5405db82c3cd"
      releaseId = containersRelease ^. #releaseId
      packageId = base ^. #packageId
      requirement = ">=4.6 && <5"
   in Requirement{..}

containersDepOnArray :: Requirement
containersDepOnArray =
  let requirementId = RequirementId $ read "25c5835c-4a16-11ec-a2b6-5405db82c3cd"
      releaseId = containersRelease ^. #releaseId
      packageId = array ^. #packageId
      requirement = ">=0.4.0.0"
   in Requirement{..}

containersDepOnDeepseq :: Requirement
containersDepOnDeepseq =
  let requirementId = RequirementId $ read "33d032a8-4a16-11ec-90f1-5405db82c3cd"
      releaseId = containersRelease ^. #releaseId
      packageId = deepseq ^. #packageId
      requirement = ">=1.2 && <1.5"
   in Requirement{..}

-- integer-gmp

integerGmp :: Package
integerGmp =
  let packageId = PackageId (read "85221662-4a16-11ec-8723-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "integer-gmp"
      synopsis = "This package provides the low-level implementation of the standard Integer type based on the GNU Multiple Precision Arithmetic Library (GMP)."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = ""
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/integerGmp/docs"
        , bugTracker = Nothing
        }
   in Package{..}

integerGmpRelease :: Release
integerGmpRelease =
  let releaseId = ReleaseId $ read "c52f130e-4a16-11ec-bdf4-5405db82c3cd"
      packageId = integerGmp ^. #packageId
      version   = fromJust $ simpleParsec "1.0.3.0"
      archiveChecksum = "f6104dd58898aa811ab6f646b0a169f24d33dcdc06584c0249b82e99a064a09c"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

integerGmpDepOnGhcPrim :: Requirement
integerGmpDepOnGhcPrim =
  let requirementId = RequirementId $ read "98cbb8ce-4a18-11ec-9119-5405db82c3cd"
      releaseId = integerGmpRelease ^. #releaseId
      packageId = ghcPrim ^. #packageId
      requirement = ">=0.6.1 && <0.7"
   in Requirement{..}

-- bytestring

bytestring :: Package
bytestring =
  let packageId = PackageId (read "0ff3a60c-4a17-11ec-82fa-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "bytestring"
      synopsis = "Fast, compact, strict and lazy byte strings with a list interface"
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://github.com/haskell/bytestring"
        , homepage = Just "https://github.com/haskell/bytestring"
        , documentation = "https://flora.pm/packages/@haskell/bytestring/docs"
        , bugTracker = Just "https://github.com/haskell/bytestring/issues"
        }
   in Package{..}

bytestringRelease :: Release
bytestringRelease =
  let releaseId = ReleaseId $ read "38a652ca-4a17-11ec-8b0c-5405db82c3cd"
      packageId = bytestring ^. #packageId
      version   = fromJust $ simpleParsec "0.11.1.0"
      archiveChecksum = "890e2387578f2b7948ee4dc9f7faa55add3bd08aa8e9637d5e860c4b5c9de4f4"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

bytestringDepOnBase :: Requirement
bytestringDepOnBase =
  let requirementId = RequirementId $ read "5beb4ab0-4a17-11ec-96f9-5405db82c3cd"
      releaseId = bytestringRelease ^. #releaseId
      packageId = base ^. #packageId
      requirement = ">=4.3 && <5"
   in Requirement{..}

bytestringDepOnDeepseq :: Requirement
bytestringDepOnDeepseq =
  let requirementId = RequirementId $ read "7699841c-4a17-11ec-86b6-5405db82c3cd"
      releaseId = bytestringRelease ^. #releaseId
      packageId = deepseq ^. #packageId
      requirement = ">=0"
   in Requirement{..}

bytestringDepOnGhcBignum :: Requirement
bytestringDepOnGhcBignum =
  let requirementId = RequirementId $ read "99de7946-4a17-11ec-973c-5405db82c3cd"
      releaseId = bytestringRelease ^. #releaseId
      packageId = ghcBignum ^. #packageId
      requirement = ">=1.0"
   in Requirement{..}

bytestringDepOnGhcPrim :: Requirement
bytestringDepOnGhcPrim =
  let requirementId = RequirementId $ read "e4986f88-4a16-11ec-b77f-5405db82c3cd"
      releaseId = bytestringRelease ^. #releaseId
      packageId = ghcPrim ^. #packageId
      requirement = ">=0"
   in Requirement{..}

bytestringDepOnIntegerGmp :: Requirement
bytestringDepOnIntegerGmp =
  let requirementId = RequirementId $ read "ca2e77c2-4a17-11ec-aa03-5405db82c3cd"
      releaseId = bytestringRelease ^. #releaseId
      packageId = integerGmp ^. #packageId
      requirement = ">=0.2"
   in Requirement{..}

-- binary

binary :: Package
binary =
  let packageId = PackageId (read "604e9038-4a23-11ec-a23f-5405db82c3cd")
      namespace = fromJust $ parseNamespace "hackage"
      name = fromJust $ parsePackageName "binary"
      synopsis = "Binary serialisation for Haskell values using lazy ByteStrings"
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = " https://github.com/kolmodin/binary"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@hackage/binary/docs"
        , bugTracker = Just " https://github.com/kolmodin/binary/issues"
        }
   in Package{..}

binaryRelease :: Release
binaryRelease =
  let releaseId = ReleaseId $ read "ac743c88-4a23-11ec-bdd8-5405db82c3cd"
      packageId = binary ^. #packageId
      version   = fromJust $ simpleParsec "0.10.0.0"
      archiveChecksum = "0e7be3ca38fcb23dfc7709014a9eb065e0a3e6006982a19b05a9acbdc98bef56"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

binaryDepOnArray :: Requirement
binaryDepOnArray =
  let requirementId = RequirementId $ read "e281fe96-4a23-11ec-8ba1-5405db82c3cd"
      releaseId = binaryRelease ^. #releaseId
      packageId = array ^. #packageId
      requirement = ">=0"
   in Requirement{..}

binaryDepOnBase :: Requirement
binaryDepOnBase =
  let requirementId = RequirementId $ read "09d949ea-4a24-11ec-bb9d-5405db82c3cd"
      releaseId = binaryRelease ^. #releaseId
      packageId = base ^. #packageId
      requirement = ">=4.5.0.0 && <4.13"
   in Requirement{..}

binaryDepOnBytestring :: Requirement
binaryDepOnBytestring =
  let requirementId = RequirementId $ read "25d310ea-4a24-11ec-b35b-5405db82c3cd"
      releaseId = binaryRelease ^. #releaseId
      packageId = bytestring ^. #packageId
      requirement = ">=0.10.4"
   in Requirement{..}

binaryDepOnContainers :: Requirement
binaryDepOnContainers =
  let requirementId = RequirementId $ read "4848494c-4a24-11ec-a6c4-5405db82c3cd"
      releaseId = binaryRelease ^. #releaseId
      packageId = containers ^. #packageId
      requirement = ">=0"
   in Requirement{..}

binaryDepOnGhcPrim :: Requirement
binaryDepOnGhcPrim =
  let requirementId = RequirementId $ read "3b638674-4a24-11ec-9665-5405db82c3cd"
      releaseId = binaryRelease ^. #releaseId
      packageId = ghcPrim ^. #packageId
      requirement = ">=0"
   in Requirement{..}

-- nats

nats :: Package
nats =
  let packageId = PackageId (read "caf1f610-4a13-11ec-8670-5405db82c3cd")
      namespace = fromJust $ parseNamespace "kmett"
      name = fromJust $ parsePackageName "nats"
      synopsis = "Natural numbers."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://github.com/ekmett/nats.git"
        , homepage = Just "http://github.com/ekmett/nats/"
        , documentation = "https://flora.pm/packages/@kmett/nats/docs"
        , bugTracker = Just "http://github.com/ekmett/nats/issues"
        }
   in Package{..}

natsRelease111 :: Release
natsRelease111 =
  let releaseId = ReleaseId $ read "0344de7e-4a14-11ec-be03-5405db82c3cd"
      packageId = nats ^. #packageId
      version   = fromJust $ simpleParsec "1.1.1"
      archiveChecksum = "a75b28d6aa7cff2a5c10fff87b80e3335777e58a2f70dc453805b56cd645fc32"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

nats111DepOnBase :: Requirement
nats111DepOnBase =
  let requirementId = RequirementId $ read "89d32702-4a14-11ec-bd8b-5405db82c3cd"
      releaseId = natsRelease111 ^. #releaseId
      packageId = base ^. #packageId
      requirement = ">=2 && <5"
   in Requirement{..}

natsRelease112 :: Release
natsRelease112 =
  let releaseId = ReleaseId $ read "0344de7e-4a14-11ec-be03-5405db82c3cd"
      packageId = nats ^. #packageId
      version   = fromJust $ simpleParsec "1.1.2"
      archiveChecksum = "220dbb525b4440d0385debf56493611067032fd5c702b924643bb6738792c2b7"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

-- stm

stm :: Package
stm =
  let packageId = PackageId (read "31fc55bc-44c4-11ec-b940-5405db82c3cd")
      namespace = fromJust $ parseNamespace "haskell"
      name = fromJust $ parsePackageName "stm"
      synopsis = "Software Transactional Memory, or STM, is an abstraction for concurrent communication. The main benefits of STM are composability and modularity. That is, using STM you can write concurrent abstractions that can be easily composed with any other abstraction built using STM, without exposing the details of how your abstraction ensures safety. This is typically not the case with other forms of concurrent communication, such as locks or MVars."
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://github.com/haskell/stm.git"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@haskell/stm/docs"
        , bugTracker = Just "https://github.com/haskell/stm/issues"
        }
   in Package{..}

stmRelease :: Release
stmRelease =
  let releaseId = ReleaseId $ read "4e0ddba4-44c4-11ec-9f98-5405db82c3cd"
      packageId = stm ^. #packageId
      version   = fromJust $ simpleParsec "2.5.0.1"
      archiveChecksum = "b0d20ca2303f72135a736f30894831a7c1ba4dd0218a06547c436bac979fc2b2"
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

stmDepOnBase :: Requirement
stmDepOnBase =
  let requirementId = RequirementId $ read "691fea0e-44c4-11ec-9221-5405db82c3cd"
      releaseId = stmRelease ^. #releaseId
      packageId = base ^. #packageId
      requirement = ">=4.3 && <4.15"
   in Requirement{..}

stmDepOnArray :: Requirement
stmDepOnArray =
  let requirementId = RequirementId $ read "9124d960-44c4-11ec-a693-5405db82c3cd"
      releaseId = stmRelease ^. #releaseId
      packageId = array ^. #packageId
      requirement = ">=0.3 && <0.6"
   in Requirement{..}


-- package1

package1 :: Package
package1 =
  let packageId = PackageId (read "452550b4-3f22-11ec-a150-5405db82c3cd")
      namespace = fromJust $ parseNamespace "tchoutri"
      name = fromJust $ parsePackageName "acme-unexpected"
      synopsis = "Nobody expects the ACME corporation!"
      ownerId = user1 ^. #userId
      createdAt = read "2021-04-23 11:00:00 UTC"
      updatedAt = read "2021-04-23 11:00:00 UTC"
      metadata = PackageMetadata
        { license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
        , sourceRepo = "https://github.com/tchoutri/package1"
        , homepage = Nothing
        , documentation = "https://flora.pm/packages/@tchoutri/package1/docs"
        , bugTracker = Just "https://github.com/tchoutri/package1/issues"
        }
   in Package{..}

release1 :: Release
release1 =
  let releaseId = ReleaseId (read "13e5ec30-3fb7-11ec-95cc-5405db82c3cd")
      packageId = (package1 ^. #packageId)
      version = fromJust $ simpleParsec "1.3.0.0"
      archiveChecksum = "fd5cae24737c857c4e3ab23a8d905cf9959096dae9d28499c96ee1156ca69b2d"
      createdAt = read "2021-11-07 13:31:34 UTC"
      updatedAt = read "2021-11-07 13:31:34 UTC"
   in Release{..}
