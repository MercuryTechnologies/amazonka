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
-- Module      : Amazonka.GameLift.Types.Script
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.Script where

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types.S3Location
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Properties describing a Realtime script.
--
-- __Related actions__
--
-- CreateScript | ListScripts | DescribeScript | UpdateScript |
-- DeleteScript |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newScript' smart constructor.
data Script = Script'
  { -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Core.POSIX,
    storageLocation :: Prelude.Maybe S3Location,
    -- | A descriptive label that is associated with a script. Script names do
    -- not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the Realtime script
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | Version information that is associated with a build or script. Version
    -- strings do not need to be unique.
    version :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift script resource and uniquely identifies
    -- it. ARNs are unique across all Regions. In a GameLift script ARN, the
    -- resource ID matches the /ScriptId/ value.
    scriptArn :: Prelude.Maybe Prelude.Text,
    -- | The file size of the uploaded Realtime script, expressed in bytes. When
    -- files are uploaded from an S3 location, this value remains at \"0\".
    sizeOnDisk :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Script' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'script_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'storageLocation', 'script_storageLocation' - Undocumented member.
--
-- 'name', 'script_name' - A descriptive label that is associated with a script. Script names do
-- not need to be unique.
--
-- 'scriptId', 'script_scriptId' - A unique identifier for the Realtime script
--
-- 'version', 'script_version' - Version information that is associated with a build or script. Version
-- strings do not need to be unique.
--
-- 'scriptArn', 'script_scriptArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift script resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift script ARN, the
-- resource ID matches the /ScriptId/ value.
--
-- 'sizeOnDisk', 'script_sizeOnDisk' - The file size of the uploaded Realtime script, expressed in bytes. When
-- files are uploaded from an S3 location, this value remains at \"0\".
newScript ::
  Script
newScript =
  Script'
    { creationTime = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      name = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      version = Prelude.Nothing,
      scriptArn = Prelude.Nothing,
      sizeOnDisk = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
script_creationTime :: Lens.Lens' Script (Prelude.Maybe Prelude.UTCTime)
script_creationTime = Lens.lens (\Script' {creationTime} -> creationTime) (\s@Script' {} a -> s {creationTime = a} :: Script) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
script_storageLocation :: Lens.Lens' Script (Prelude.Maybe S3Location)
script_storageLocation = Lens.lens (\Script' {storageLocation} -> storageLocation) (\s@Script' {} a -> s {storageLocation = a} :: Script)

-- | A descriptive label that is associated with a script. Script names do
-- not need to be unique.
script_name :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_name = Lens.lens (\Script' {name} -> name) (\s@Script' {} a -> s {name = a} :: Script)

-- | A unique identifier for the Realtime script
script_scriptId :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_scriptId = Lens.lens (\Script' {scriptId} -> scriptId) (\s@Script' {} a -> s {scriptId = a} :: Script)

-- | Version information that is associated with a build or script. Version
-- strings do not need to be unique.
script_version :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_version = Lens.lens (\Script' {version} -> version) (\s@Script' {} a -> s {version = a} :: Script)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift script resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift script ARN, the
-- resource ID matches the /ScriptId/ value.
script_scriptArn :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_scriptArn = Lens.lens (\Script' {scriptArn} -> scriptArn) (\s@Script' {} a -> s {scriptArn = a} :: Script)

-- | The file size of the uploaded Realtime script, expressed in bytes. When
-- files are uploaded from an S3 location, this value remains at \"0\".
script_sizeOnDisk :: Lens.Lens' Script (Prelude.Maybe Prelude.Natural)
script_sizeOnDisk = Lens.lens (\Script' {sizeOnDisk} -> sizeOnDisk) (\s@Script' {} a -> s {sizeOnDisk = a} :: Script)

instance Core.FromJSON Script where
  parseJSON =
    Core.withObject
      "Script"
      ( \x ->
          Script'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "StorageLocation")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ScriptId")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "ScriptArn")
            Prelude.<*> (x Core..:? "SizeOnDisk")
      )

instance Prelude.Hashable Script where
  hashWithSalt salt' Script' {..} =
    salt' `Prelude.hashWithSalt` sizeOnDisk
      `Prelude.hashWithSalt` scriptArn
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` scriptId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` storageLocation
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData Script where
  rnf Script' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sizeOnDisk
      `Prelude.seq` Prelude.rnf scriptArn
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf scriptId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf storageLocation
