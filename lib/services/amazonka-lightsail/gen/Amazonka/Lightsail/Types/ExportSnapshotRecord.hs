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
-- Module      : Amazonka.Lightsail.Types.ExportSnapshotRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ExportSnapshotRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.DestinationInfo
import Amazonka.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Amazonka.Lightsail.Types.RecordState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes an export snapshot record.
--
-- /See:/ 'newExportSnapshotRecord' smart constructor.
data ExportSnapshotRecord = ExportSnapshotRecord'
  { -- | The state of the export snapshot record.
    state :: Prelude.Maybe RecordState,
    -- | A list of objects describing the destination of the export snapshot
    -- record.
    destinationInfo :: Prelude.Maybe DestinationInfo,
    -- | The Lightsail resource type (e.g., @ExportSnapshotRecord@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the export snapshot record.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the export snapshot record was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The AWS Region and Availability Zone where the export snapshot record is
    -- located.
    location :: Prelude.Maybe ResourceLocation,
    -- | The export snapshot record name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of objects describing the source of the export snapshot record.
    sourceInfo :: Prelude.Maybe ExportSnapshotRecordSourceInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSnapshotRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'exportSnapshotRecord_state' - The state of the export snapshot record.
--
-- 'destinationInfo', 'exportSnapshotRecord_destinationInfo' - A list of objects describing the destination of the export snapshot
-- record.
--
-- 'resourceType', 'exportSnapshotRecord_resourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@).
--
-- 'arn', 'exportSnapshotRecord_arn' - The Amazon Resource Name (ARN) of the export snapshot record.
--
-- 'createdAt', 'exportSnapshotRecord_createdAt' - The date when the export snapshot record was created.
--
-- 'location', 'exportSnapshotRecord_location' - The AWS Region and Availability Zone where the export snapshot record is
-- located.
--
-- 'name', 'exportSnapshotRecord_name' - The export snapshot record name.
--
-- 'sourceInfo', 'exportSnapshotRecord_sourceInfo' - A list of objects describing the source of the export snapshot record.
newExportSnapshotRecord ::
  ExportSnapshotRecord
newExportSnapshotRecord =
  ExportSnapshotRecord'
    { state = Prelude.Nothing,
      destinationInfo = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceInfo = Prelude.Nothing
    }

-- | The state of the export snapshot record.
exportSnapshotRecord_state :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe RecordState)
exportSnapshotRecord_state = Lens.lens (\ExportSnapshotRecord' {state} -> state) (\s@ExportSnapshotRecord' {} a -> s {state = a} :: ExportSnapshotRecord)

-- | A list of objects describing the destination of the export snapshot
-- record.
exportSnapshotRecord_destinationInfo :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe DestinationInfo)
exportSnapshotRecord_destinationInfo = Lens.lens (\ExportSnapshotRecord' {destinationInfo} -> destinationInfo) (\s@ExportSnapshotRecord' {} a -> s {destinationInfo = a} :: ExportSnapshotRecord)

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@).
exportSnapshotRecord_resourceType :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe ResourceType)
exportSnapshotRecord_resourceType = Lens.lens (\ExportSnapshotRecord' {resourceType} -> resourceType) (\s@ExportSnapshotRecord' {} a -> s {resourceType = a} :: ExportSnapshotRecord)

-- | The Amazon Resource Name (ARN) of the export snapshot record.
exportSnapshotRecord_arn :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe Prelude.Text)
exportSnapshotRecord_arn = Lens.lens (\ExportSnapshotRecord' {arn} -> arn) (\s@ExportSnapshotRecord' {} a -> s {arn = a} :: ExportSnapshotRecord)

-- | The date when the export snapshot record was created.
exportSnapshotRecord_createdAt :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe Prelude.UTCTime)
exportSnapshotRecord_createdAt = Lens.lens (\ExportSnapshotRecord' {createdAt} -> createdAt) (\s@ExportSnapshotRecord' {} a -> s {createdAt = a} :: ExportSnapshotRecord) Prelude.. Lens.mapping Core._Time

-- | The AWS Region and Availability Zone where the export snapshot record is
-- located.
exportSnapshotRecord_location :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe ResourceLocation)
exportSnapshotRecord_location = Lens.lens (\ExportSnapshotRecord' {location} -> location) (\s@ExportSnapshotRecord' {} a -> s {location = a} :: ExportSnapshotRecord)

-- | The export snapshot record name.
exportSnapshotRecord_name :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe Prelude.Text)
exportSnapshotRecord_name = Lens.lens (\ExportSnapshotRecord' {name} -> name) (\s@ExportSnapshotRecord' {} a -> s {name = a} :: ExportSnapshotRecord)

-- | A list of objects describing the source of the export snapshot record.
exportSnapshotRecord_sourceInfo :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe ExportSnapshotRecordSourceInfo)
exportSnapshotRecord_sourceInfo = Lens.lens (\ExportSnapshotRecord' {sourceInfo} -> sourceInfo) (\s@ExportSnapshotRecord' {} a -> s {sourceInfo = a} :: ExportSnapshotRecord)

instance Core.FromJSON ExportSnapshotRecord where
  parseJSON =
    Core.withObject
      "ExportSnapshotRecord"
      ( \x ->
          ExportSnapshotRecord'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "destinationInfo")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "sourceInfo")
      )

instance Prelude.Hashable ExportSnapshotRecord where
  hashWithSalt salt' ExportSnapshotRecord' {..} =
    salt' `Prelude.hashWithSalt` sourceInfo
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` destinationInfo
      `Prelude.hashWithSalt` state

instance Prelude.NFData ExportSnapshotRecord where
  rnf ExportSnapshotRecord' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf sourceInfo
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf destinationInfo
