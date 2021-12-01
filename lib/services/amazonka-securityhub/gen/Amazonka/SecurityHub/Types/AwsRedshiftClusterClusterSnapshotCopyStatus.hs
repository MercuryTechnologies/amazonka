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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a cross-Region snapshot copy.
--
-- /See:/ 'newAwsRedshiftClusterClusterSnapshotCopyStatus' smart constructor.
data AwsRedshiftClusterClusterSnapshotCopyStatus = AwsRedshiftClusterClusterSnapshotCopyStatus'
  { -- | The number of days that manual snapshots are retained in the destination
    -- region after they are copied from a source region.
    --
    -- If the value is -1, then the manual snapshot is retained indefinitely.
    --
    -- Valid values: Either -1 or an integer between 1 and 3,653
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The number of days to retain automated snapshots in the destination
    -- Region after they are copied from a source Region.
    retentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The destination Region that snapshots are automatically copied to when
    -- cross-Region snapshot copy is enabled.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterClusterSnapshotCopyStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manualSnapshotRetentionPeriod', 'awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod' - The number of days that manual snapshots are retained in the destination
-- region after they are copied from a source region.
--
-- If the value is -1, then the manual snapshot is retained indefinitely.
--
-- Valid values: Either -1 or an integer between 1 and 3,653
--
-- 'retentionPeriod', 'awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod' - The number of days to retain automated snapshots in the destination
-- Region after they are copied from a source Region.
--
-- 'destinationRegion', 'awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion' - The destination Region that snapshots are automatically copied to when
-- cross-Region snapshot copy is enabled.
--
-- 'snapshotCopyGrantName', 'awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName' - The name of the snapshot copy grant.
newAwsRedshiftClusterClusterSnapshotCopyStatus ::
  AwsRedshiftClusterClusterSnapshotCopyStatus
newAwsRedshiftClusterClusterSnapshotCopyStatus =
  AwsRedshiftClusterClusterSnapshotCopyStatus'
    { manualSnapshotRetentionPeriod =
        Prelude.Nothing,
      retentionPeriod =
        Prelude.Nothing,
      destinationRegion =
        Prelude.Nothing,
      snapshotCopyGrantName =
        Prelude.Nothing
    }

-- | The number of days that manual snapshots are retained in the destination
-- region after they are copied from a source region.
--
-- If the value is -1, then the manual snapshot is retained indefinitely.
--
-- Valid values: Either -1 or an integer between 1 and 3,653
awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Int)
awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {manualSnapshotRetentionPeriod = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

-- | The number of days to retain automated snapshots in the destination
-- Region after they are copied from a source Region.
awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Int)
awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {retentionPeriod} -> retentionPeriod) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {retentionPeriod = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

-- | The destination Region that snapshots are automatically copied to when
-- cross-Region snapshot copy is enabled.
awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {destinationRegion} -> destinationRegion) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {destinationRegion = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

-- | The name of the snapshot copy grant.
awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName :: Lens.Lens' AwsRedshiftClusterClusterSnapshotCopyStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName = Lens.lens (\AwsRedshiftClusterClusterSnapshotCopyStatus' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@AwsRedshiftClusterClusterSnapshotCopyStatus' {} a -> s {snapshotCopyGrantName = a} :: AwsRedshiftClusterClusterSnapshotCopyStatus)

instance
  Core.FromJSON
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterClusterSnapshotCopyStatus"
      ( \x ->
          AwsRedshiftClusterClusterSnapshotCopyStatus'
            Prelude.<$> (x Core..:? "ManualSnapshotRetentionPeriod")
              Prelude.<*> (x Core..:? "RetentionPeriod")
              Prelude.<*> (x Core..:? "DestinationRegion")
              Prelude.<*> (x Core..:? "SnapshotCopyGrantName")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  hashWithSalt
    salt'
    AwsRedshiftClusterClusterSnapshotCopyStatus' {..} =
      salt' `Prelude.hashWithSalt` snapshotCopyGrantName
        `Prelude.hashWithSalt` destinationRegion
        `Prelude.hashWithSalt` retentionPeriod
        `Prelude.hashWithSalt` manualSnapshotRetentionPeriod

instance
  Prelude.NFData
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  rnf AwsRedshiftClusterClusterSnapshotCopyStatus' {..} =
    Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf snapshotCopyGrantName
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf retentionPeriod

instance
  Core.ToJSON
    AwsRedshiftClusterClusterSnapshotCopyStatus
  where
  toJSON
    AwsRedshiftClusterClusterSnapshotCopyStatus' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ManualSnapshotRetentionPeriod" Core..=)
                Prelude.<$> manualSnapshotRetentionPeriod,
              ("RetentionPeriod" Core..=)
                Prelude.<$> retentionPeriod,
              ("DestinationRegion" Core..=)
                Prelude.<$> destinationRegion,
              ("SnapshotCopyGrantName" Core..=)
                Prelude.<$> snapshotCopyGrantName
            ]
        )
