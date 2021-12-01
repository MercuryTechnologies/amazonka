{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataSync.Types.LocationListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.LocationListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a single entry in a list of locations. @LocationListEntry@
-- returns an array that contains a list of locations when the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListLocations.html ListLocations>
-- operation is called.
--
-- /See:/ 'newLocationListEntry' smart constructor.
data LocationListEntry = LocationListEntry'
  { -- | Represents a list of URLs of a location. @LocationUri@ returns an array
    -- that contains a list of locations when the
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListLocations.html ListLocations>
    -- operation is called.
    --
    -- Format: @TYPE:\/\/GLOBAL_ID\/SUBDIR@.
    --
    -- TYPE designates the type of location. Valid values: NFS | EFS | S3.
    --
    -- GLOBAL_ID is the globally unique identifier of the resource that backs
    -- the location. An example for EFS is @us-east-2.fs-abcd1234@. An example
    -- for Amazon S3 is the bucket name, such as @myBucket@. An example for NFS
    -- is a valid IPv4 address or a host name compliant with Domain Name
    -- Service (DNS).
    --
    -- SUBDIR is a valid file system path, delimited by forward slashes as is
    -- the *nix convention. For NFS and Amazon EFS, it\'s the export path to
    -- mount the location. For Amazon S3, it\'s the prefix path that you mount
    -- to and treat as the root of the location.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the location. For Network File System
    -- (NFS) or Amazon EFS, the location is the export path. For Amazon S3, the
    -- location is the prefix path that you want to mount and use as the root
    -- of the location.
    locationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationUri', 'locationListEntry_locationUri' - Represents a list of URLs of a location. @LocationUri@ returns an array
-- that contains a list of locations when the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListLocations.html ListLocations>
-- operation is called.
--
-- Format: @TYPE:\/\/GLOBAL_ID\/SUBDIR@.
--
-- TYPE designates the type of location. Valid values: NFS | EFS | S3.
--
-- GLOBAL_ID is the globally unique identifier of the resource that backs
-- the location. An example for EFS is @us-east-2.fs-abcd1234@. An example
-- for Amazon S3 is the bucket name, such as @myBucket@. An example for NFS
-- is a valid IPv4 address or a host name compliant with Domain Name
-- Service (DNS).
--
-- SUBDIR is a valid file system path, delimited by forward slashes as is
-- the *nix convention. For NFS and Amazon EFS, it\'s the export path to
-- mount the location. For Amazon S3, it\'s the prefix path that you mount
-- to and treat as the root of the location.
--
-- 'locationArn', 'locationListEntry_locationArn' - The Amazon Resource Name (ARN) of the location. For Network File System
-- (NFS) or Amazon EFS, the location is the export path. For Amazon S3, the
-- location is the prefix path that you want to mount and use as the root
-- of the location.
newLocationListEntry ::
  LocationListEntry
newLocationListEntry =
  LocationListEntry'
    { locationUri = Prelude.Nothing,
      locationArn = Prelude.Nothing
    }

-- | Represents a list of URLs of a location. @LocationUri@ returns an array
-- that contains a list of locations when the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListLocations.html ListLocations>
-- operation is called.
--
-- Format: @TYPE:\/\/GLOBAL_ID\/SUBDIR@.
--
-- TYPE designates the type of location. Valid values: NFS | EFS | S3.
--
-- GLOBAL_ID is the globally unique identifier of the resource that backs
-- the location. An example for EFS is @us-east-2.fs-abcd1234@. An example
-- for Amazon S3 is the bucket name, such as @myBucket@. An example for NFS
-- is a valid IPv4 address or a host name compliant with Domain Name
-- Service (DNS).
--
-- SUBDIR is a valid file system path, delimited by forward slashes as is
-- the *nix convention. For NFS and Amazon EFS, it\'s the export path to
-- mount the location. For Amazon S3, it\'s the prefix path that you mount
-- to and treat as the root of the location.
locationListEntry_locationUri :: Lens.Lens' LocationListEntry (Prelude.Maybe Prelude.Text)
locationListEntry_locationUri = Lens.lens (\LocationListEntry' {locationUri} -> locationUri) (\s@LocationListEntry' {} a -> s {locationUri = a} :: LocationListEntry)

-- | The Amazon Resource Name (ARN) of the location. For Network File System
-- (NFS) or Amazon EFS, the location is the export path. For Amazon S3, the
-- location is the prefix path that you want to mount and use as the root
-- of the location.
locationListEntry_locationArn :: Lens.Lens' LocationListEntry (Prelude.Maybe Prelude.Text)
locationListEntry_locationArn = Lens.lens (\LocationListEntry' {locationArn} -> locationArn) (\s@LocationListEntry' {} a -> s {locationArn = a} :: LocationListEntry)

instance Core.FromJSON LocationListEntry where
  parseJSON =
    Core.withObject
      "LocationListEntry"
      ( \x ->
          LocationListEntry'
            Prelude.<$> (x Core..:? "LocationUri")
            Prelude.<*> (x Core..:? "LocationArn")
      )

instance Prelude.Hashable LocationListEntry where
  hashWithSalt salt' LocationListEntry' {..} =
    salt' `Prelude.hashWithSalt` locationArn
      `Prelude.hashWithSalt` locationUri

instance Prelude.NFData LocationListEntry where
  rnf LocationListEntry' {..} =
    Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf locationArn
