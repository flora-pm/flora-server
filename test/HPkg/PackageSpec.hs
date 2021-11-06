module HPkg.PackageSpec where

import Data.Password.Argon2
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldReturn)
import Distribution.SPDX.License
import Distribution.SPDX.LicenseId

import SpecHelpers (migrate)
import HPkg.Model.User
import HPkg.Model.Package
import Distribution.SPDX

user1 :: User
user1 =
  let userId      = UserId (read "4e511d7a-a464-11eb-b30b-5405db82c3cd")
      username    = "pmpc"
      email       = "pmpc@example.com"
      displayName = "Plonk McPlonkface"
      password    = PasswordHash "foobar2000"
      createdAt   = read "2021-04-23 10:00:00 UTC"
      updatedAt   = read "2021-04-23 10:00:00 UTC"
  in  User { .. }

package1 :: Package
package1 =
  let packageId = PackageId (read "452550b4-3f22-11ec-a150-5405db82c3cd")
      name = "acme-unexpected"
      synopsis = "Nobody expects the ACME corporation!"
      license = License (ELicense (ELicenseId BSD_3_Clause) Nothing)
      ownerId = userId user1
      createdAt = read "2021-04-23 11:00:00 UTC"
      updatedAt = read "2021-04-23 11:00:00 UTC"
   in Package{..}

spec :: Spec
spec = describeDB migrate "packages" $ do
  itDB "Insert a package and fetch it" $ do
    insertUser user1
    getUserById (userId user1) `shouldReturn` Just user1
