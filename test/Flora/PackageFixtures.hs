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
      createdAt = read "2021-10-03 11:30:00 UTC"
      updatedAt = read "2021-10-03 11:30:00 UTC"
   in Release{..}

-- ghc-bignum

ghcBignum :: Package
ghcBignum =
  let packageId = PackageId (read "b845b706-3fc1-11ec-bf77-5405db82c3cd")
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

-- package1

package1 :: Package
package1 =
  let packageId = PackageId (read "452550b4-3f22-11ec-a150-5405db82c3cd")
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
      createdAt = read "2021-11-07 13:31:34 UTC"
      updatedAt = read "2021-11-07 13:31:34 UTC"
   in Release{..}

