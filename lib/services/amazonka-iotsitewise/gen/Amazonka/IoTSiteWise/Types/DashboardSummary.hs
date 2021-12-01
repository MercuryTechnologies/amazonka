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
-- Module      : Amazonka.IoTSiteWise.Types.DashboardSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.DashboardSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a dashboard summary.
--
-- /See:/ 'newDashboardSummary' smart constructor.
data DashboardSummary = DashboardSummary'
  { -- | The date the dashboard was last updated, in Unix epoch time.
    lastUpdateDate :: Prelude.Maybe Core.POSIX,
    -- | The date the dashboard was created, in Unix epoch time.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The dashboard\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dashboard.
    id :: Prelude.Text,
    -- | The name of the dashboard
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateDate', 'dashboardSummary_lastUpdateDate' - The date the dashboard was last updated, in Unix epoch time.
--
-- 'creationDate', 'dashboardSummary_creationDate' - The date the dashboard was created, in Unix epoch time.
--
-- 'description', 'dashboardSummary_description' - The dashboard\'s description.
--
-- 'id', 'dashboardSummary_id' - The ID of the dashboard.
--
-- 'name', 'dashboardSummary_name' - The name of the dashboard
newDashboardSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DashboardSummary
newDashboardSummary pId_ pName_ =
  DashboardSummary'
    { lastUpdateDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_,
      name = pName_
    }

-- | The date the dashboard was last updated, in Unix epoch time.
dashboardSummary_lastUpdateDate :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.UTCTime)
dashboardSummary_lastUpdateDate = Lens.lens (\DashboardSummary' {lastUpdateDate} -> lastUpdateDate) (\s@DashboardSummary' {} a -> s {lastUpdateDate = a} :: DashboardSummary) Prelude.. Lens.mapping Core._Time

-- | The date the dashboard was created, in Unix epoch time.
dashboardSummary_creationDate :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.UTCTime)
dashboardSummary_creationDate = Lens.lens (\DashboardSummary' {creationDate} -> creationDate) (\s@DashboardSummary' {} a -> s {creationDate = a} :: DashboardSummary) Prelude.. Lens.mapping Core._Time

-- | The dashboard\'s description.
dashboardSummary_description :: Lens.Lens' DashboardSummary (Prelude.Maybe Prelude.Text)
dashboardSummary_description = Lens.lens (\DashboardSummary' {description} -> description) (\s@DashboardSummary' {} a -> s {description = a} :: DashboardSummary)

-- | The ID of the dashboard.
dashboardSummary_id :: Lens.Lens' DashboardSummary Prelude.Text
dashboardSummary_id = Lens.lens (\DashboardSummary' {id} -> id) (\s@DashboardSummary' {} a -> s {id = a} :: DashboardSummary)

-- | The name of the dashboard
dashboardSummary_name :: Lens.Lens' DashboardSummary Prelude.Text
dashboardSummary_name = Lens.lens (\DashboardSummary' {name} -> name) (\s@DashboardSummary' {} a -> s {name = a} :: DashboardSummary)

instance Core.FromJSON DashboardSummary where
  parseJSON =
    Core.withObject
      "DashboardSummary"
      ( \x ->
          DashboardSummary'
            Prelude.<$> (x Core..:? "lastUpdateDate")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable DashboardSummary where
  hashWithSalt salt' DashboardSummary' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastUpdateDate

instance Prelude.NFData DashboardSummary where
  rnf DashboardSummary' {..} =
    Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationDate
