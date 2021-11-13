module Flora.PackageFixtures where

import Data.Maybe
import Distribution.Parsec
import Distribution.SPDX
import Optics.Core

import Flora.Model.Package
import Flora.Model.Release
import Flora.Model.Requirement
import Flora.Model.User
import Flora.UserFixtures

-- ghc-prim

ghcPrim :: Package
ghcPrim =
  let packageId = PackageId (read "7a1ae448-3fc0-11ec-a038-5405db82c3cd")
      namespace = "haskell"
      name = "ghc-prim"
      synopsis = "This package contains the primitive types and operations supplied by GHC."
      license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
      ownerId = ben ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
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
      namespace = "haskell"
      name = "ghc-bignum"
      synopsis = "This package provides the low-level implementation of the standard BigNat, Natural and Integer types."
      license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
      ownerId = syl20 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
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
      namespace = "haskell"
      name = "base"
      synopsis = "This package contains the Standard Haskell Prelude and its support libraries, and a large collection of useful libraries ranging from data structures to parsing combinators and debugging utilities."
      license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
      ownerId = ben ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
   in Package{..}

baseRelease :: Release
baseRelease =
  let releaseId = ReleaseId $ read "558568cc-3fc2-11ec-985a-5405db82c3cd"
      packageId = base ^. #packageId
      version   = fromJust $ simpleParsec "1.2"
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
      namespace = "haskell"
      name = "array"
      synopsis = "In addition to providing the Data.Array module as specified in the Haskell 2010 Language Report, this package also defines the classes IArray of immutable arrays and MArray of arrays mutable within appropriate monads, as well as some instances of these classes."
      license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
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

-- stm

stm :: Package
stm =
  let packageId = PackageId (read "31fc55bc-44c4-11ec-b940-5405db82c3cd")
      namespace = "haskell"
      name = "stm"
      synopsis = "Software Transactional Memory, or STM, is an abstraction for concurrent communication. The main benefits of STM are composability and modularity. That is, using STM you can write concurrent abstractions that can be easily composed with any other abstraction built using STM, without exposing the details of how your abstraction ensures safety. This is typically not the case with other forms of concurrent communication, such as locks or MVars."
      license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
      ownerId = user1 ^. #userId
      createdAt = read "2014-03-23 23:03:32 UTC"
      updatedAt = read "2021-11-03 11:30:00 UTC"
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
      namespace = "tchoutri"
      name = "acme-unexpected"
      synopsis = "Nobody expects the ACME corporation!"
      license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
      ownerId = UserId (read "4e511d7a-a464-11eb-b30b-5405db82c3cd")
      createdAt = read "2021-04-23 11:00:00 UTC"
      updatedAt = read "2021-04-23 11:00:00 UTC"
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
