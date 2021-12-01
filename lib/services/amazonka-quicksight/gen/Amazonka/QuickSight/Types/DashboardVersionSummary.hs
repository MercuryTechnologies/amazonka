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
-- Module      : Amazonka.QuickSight.Types.DashboardVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResourceStatus

-- | Dashboard version summary.
--
-- /See:/ 'newDashboardVersionSummary' smart constructor.
data DashboardVersionSummary = DashboardVersionSummary'
  { -- | The HTTP status of the request.
    status :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this dashboard version was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Source entity ARN.
    sourceEntityArn :: Prelude.Maybe Prelude.Text,
    -- | Version number.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | Description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dashboardVersionSummary_status' - The HTTP status of the request.
--
-- 'arn', 'dashboardVersionSummary_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'dashboardVersionSummary_createdTime' - The time that this dashboard version was created.
--
-- 'sourceEntityArn', 'dashboardVersionSummary_sourceEntityArn' - Source entity ARN.
--
-- 'versionNumber', 'dashboardVersionSummary_versionNumber' - Version number.
--
-- 'description', 'dashboardVersionSummary_description' - Description.
newDashboardVersionSummary ::
  DashboardVersionSummary
newDashboardVersionSummary =
  DashboardVersionSummary'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      sourceEntityArn = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The HTTP status of the request.
dashboardVersionSummary_status :: Lens.Lens' DashboardVersionSummary (Prelude.Maybe ResourceStatus)
dashboardVersionSummary_status = Lens.lens (\DashboardVersionSummary' {status} -> status) (\s@DashboardVersionSummary' {} a -> s {status = a} :: DashboardVersionSummary)

-- | The Amazon Resource Name (ARN) of the resource.
dashboardVersionSummary_arn :: Lens.Lens' DashboardVersionSummary (Prelude.Maybe Prelude.Text)
dashboardVersionSummary_arn = Lens.lens (\DashboardVersionSummary' {arn} -> arn) (\s@DashboardVersionSummary' {} a -> s {arn = a} :: DashboardVersionSummary)

-- | The time that this dashboard version was created.
dashboardVersionSummary_createdTime :: Lens.Lens' DashboardVersionSummary (Prelude.Maybe Prelude.UTCTime)
dashboardVersionSummary_createdTime = Lens.lens (\DashboardVersionSummary' {createdTime} -> createdTime) (\s@DashboardVersionSummary' {} a -> s {createdTime = a} :: DashboardVersionSummary) Prelude.. Lens.mapping Core._Time

-- | Source entity ARN.
dashboardVersionSummary_sourceEntityArn :: Lens.Lens' DashboardVersionSummary (Prelude.Maybe Prelude.Text)
dashboardVersionSummary_sourceEntityArn = Lens.lens (\DashboardVersionSummary' {sourceEntityArn} -> sourceEntityArn) (\s@DashboardVersionSummary' {} a -> s {sourceEntityArn = a} :: DashboardVersionSummary)

-- | Version number.
dashboardVersionSummary_versionNumber :: Lens.Lens' DashboardVersionSummary (Prelude.Maybe Prelude.Natural)
dashboardVersionSummary_versionNumber = Lens.lens (\DashboardVersionSummary' {versionNumber} -> versionNumber) (\s@DashboardVersionSummary' {} a -> s {versionNumber = a} :: DashboardVersionSummary)

-- | Description.
dashboardVersionSummary_description :: Lens.Lens' DashboardVersionSummary (Prelude.Maybe Prelude.Text)
dashboardVersionSummary_description = Lens.lens (\DashboardVersionSummary' {description} -> description) (\s@DashboardVersionSummary' {} a -> s {description = a} :: DashboardVersionSummary)

instance Core.FromJSON DashboardVersionSummary where
  parseJSON =
    Core.withObject
      "DashboardVersionSummary"
      ( \x ->
          DashboardVersionSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "SourceEntityArn")
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable DashboardVersionSummary where
  hashWithSalt salt' DashboardVersionSummary' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` sourceEntityArn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status

instance Prelude.NFData DashboardVersionSummary where
  rnf DashboardVersionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf sourceEntityArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
