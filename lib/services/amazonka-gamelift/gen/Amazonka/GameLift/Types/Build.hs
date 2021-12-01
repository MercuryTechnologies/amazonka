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
-- Module      : Amazonka.GameLift.Types.Build
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.Build where

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types.BuildStatus
import Amazonka.GameLift.Types.OperatingSystem
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Properties describing a custom game build.
--
-- __Related actions__
--
-- CreateBuild | ListBuilds | DescribeBuild | UpdateBuild | DeleteBuild |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newBuild' smart constructor.
data Build = Build'
  { -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Current status of the build.
    --
    -- Possible build statuses include the following:
    --
    -- -   __INITIALIZED__ -- A new build has been defined, but no files have
    --     been uploaded. You cannot create fleets for builds that are in this
    --     status. When a build is successfully created, the build status is
    --     set to this value.
    --
    -- -   __READY__ -- The game build has been successfully uploaded. You can
    --     now create new fleets for this build.
    --
    -- -   __FAILED__ -- The game build upload failed. You cannot create new
    --     fleets for this build.
    status :: Prelude.Maybe BuildStatus,
    -- | Operating system that the game server binaries are built to run on. This
    -- value determines the type of fleet resources that you can use for this
    -- build.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A unique identifier for the build.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a build. Build names do not
    -- need to be unique. It can be set using CreateBuild or UpdateBuild.
    name :: Prelude.Maybe Prelude.Text,
    -- | Version information that is associated with a build or script. Version
    -- strings do not need to be unique. This value can be set using
    -- CreateBuild or UpdateBuild.
    version :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift build resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::build\/build-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    -- In a GameLift build ARN, the resource ID matches the /BuildId/ value.
    buildArn :: Prelude.Maybe Prelude.Text,
    -- | File size of the uploaded game build, expressed in bytes. When the build
    -- status is @INITIALIZED@, this value is 0.
    sizeOnDisk :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Build' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'build_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'status', 'build_status' - Current status of the build.
--
-- Possible build statuses include the following:
--
-- -   __INITIALIZED__ -- A new build has been defined, but no files have
--     been uploaded. You cannot create fleets for builds that are in this
--     status. When a build is successfully created, the build status is
--     set to this value.
--
-- -   __READY__ -- The game build has been successfully uploaded. You can
--     now create new fleets for this build.
--
-- -   __FAILED__ -- The game build upload failed. You cannot create new
--     fleets for this build.
--
-- 'operatingSystem', 'build_operatingSystem' - Operating system that the game server binaries are built to run on. This
-- value determines the type of fleet resources that you can use for this
-- build.
--
-- 'buildId', 'build_buildId' - A unique identifier for the build.
--
-- 'name', 'build_name' - A descriptive label that is associated with a build. Build names do not
-- need to be unique. It can be set using CreateBuild or UpdateBuild.
--
-- 'version', 'build_version' - Version information that is associated with a build or script. Version
-- strings do not need to be unique. This value can be set using
-- CreateBuild or UpdateBuild.
--
-- 'buildArn', 'build_buildArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift build resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::build\/build-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift build ARN, the resource ID matches the /BuildId/ value.
--
-- 'sizeOnDisk', 'build_sizeOnDisk' - File size of the uploaded game build, expressed in bytes. When the build
-- status is @INITIALIZED@, this value is 0.
newBuild ::
  Build
newBuild =
  Build'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      buildId = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      buildArn = Prelude.Nothing,
      sizeOnDisk = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
build_creationTime :: Lens.Lens' Build (Prelude.Maybe Prelude.UTCTime)
build_creationTime = Lens.lens (\Build' {creationTime} -> creationTime) (\s@Build' {} a -> s {creationTime = a} :: Build) Prelude.. Lens.mapping Core._Time

-- | Current status of the build.
--
-- Possible build statuses include the following:
--
-- -   __INITIALIZED__ -- A new build has been defined, but no files have
--     been uploaded. You cannot create fleets for builds that are in this
--     status. When a build is successfully created, the build status is
--     set to this value.
--
-- -   __READY__ -- The game build has been successfully uploaded. You can
--     now create new fleets for this build.
--
-- -   __FAILED__ -- The game build upload failed. You cannot create new
--     fleets for this build.
build_status :: Lens.Lens' Build (Prelude.Maybe BuildStatus)
build_status = Lens.lens (\Build' {status} -> status) (\s@Build' {} a -> s {status = a} :: Build)

-- | Operating system that the game server binaries are built to run on. This
-- value determines the type of fleet resources that you can use for this
-- build.
build_operatingSystem :: Lens.Lens' Build (Prelude.Maybe OperatingSystem)
build_operatingSystem = Lens.lens (\Build' {operatingSystem} -> operatingSystem) (\s@Build' {} a -> s {operatingSystem = a} :: Build)

-- | A unique identifier for the build.
build_buildId :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_buildId = Lens.lens (\Build' {buildId} -> buildId) (\s@Build' {} a -> s {buildId = a} :: Build)

-- | A descriptive label that is associated with a build. Build names do not
-- need to be unique. It can be set using CreateBuild or UpdateBuild.
build_name :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_name = Lens.lens (\Build' {name} -> name) (\s@Build' {} a -> s {name = a} :: Build)

-- | Version information that is associated with a build or script. Version
-- strings do not need to be unique. This value can be set using
-- CreateBuild or UpdateBuild.
build_version :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_version = Lens.lens (\Build' {version} -> version) (\s@Build' {} a -> s {version = a} :: Build)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift build resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::build\/build-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift build ARN, the resource ID matches the /BuildId/ value.
build_buildArn :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_buildArn = Lens.lens (\Build' {buildArn} -> buildArn) (\s@Build' {} a -> s {buildArn = a} :: Build)

-- | File size of the uploaded game build, expressed in bytes. When the build
-- status is @INITIALIZED@, this value is 0.
build_sizeOnDisk :: Lens.Lens' Build (Prelude.Maybe Prelude.Natural)
build_sizeOnDisk = Lens.lens (\Build' {sizeOnDisk} -> sizeOnDisk) (\s@Build' {} a -> s {sizeOnDisk = a} :: Build)

instance Core.FromJSON Build where
  parseJSON =
    Core.withObject
      "Build"
      ( \x ->
          Build'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "OperatingSystem")
            Prelude.<*> (x Core..:? "BuildId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "BuildArn")
            Prelude.<*> (x Core..:? "SizeOnDisk")
      )

instance Prelude.Hashable Build where
  hashWithSalt salt' Build' {..} =
    salt' `Prelude.hashWithSalt` sizeOnDisk
      `Prelude.hashWithSalt` buildArn
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` buildId
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData Build where
  rnf Build' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sizeOnDisk
      `Prelude.seq` Prelude.rnf buildArn
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf buildId
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf status
