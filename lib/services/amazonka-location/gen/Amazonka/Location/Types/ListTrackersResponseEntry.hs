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
-- Module      : Amazonka.Location.Types.ListTrackersResponseEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListTrackersResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.PricingPlan
import qualified Amazonka.Prelude as Prelude

-- | Contains the tracker resource details.
--
-- /See:/ 'newListTrackersResponseEntry' smart constructor.
data ListTrackersResponseEntry = ListTrackersResponseEntry'
  { -- | The specified data provider for the tracker resource.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | The timestamp for when the tracker resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Core.POSIX,
    -- | The description for the tracker resource.
    description :: Prelude.Text,
    -- | The pricing plan for the specified tracker resource.
    --
    -- For additional details and restrictions on each pricing plan option, see
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
    pricingPlan :: PricingPlan,
    -- | The name of the tracker resource.
    trackerName :: Prelude.Text,
    -- | The timestamp at which the device\'s position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrackersResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlanDataSource', 'listTrackersResponseEntry_pricingPlanDataSource' - The specified data provider for the tracker resource.
--
-- 'createTime', 'listTrackersResponseEntry_createTime' - The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'description', 'listTrackersResponseEntry_description' - The description for the tracker resource.
--
-- 'pricingPlan', 'listTrackersResponseEntry_pricingPlan' - The pricing plan for the specified tracker resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
--
-- 'trackerName', 'listTrackersResponseEntry_trackerName' - The name of the tracker resource.
--
-- 'updateTime', 'listTrackersResponseEntry_updateTime' - The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newListTrackersResponseEntry ::
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'description'
  Prelude.Text ->
  -- | 'pricingPlan'
  PricingPlan ->
  -- | 'trackerName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListTrackersResponseEntry
newListTrackersResponseEntry
  pCreateTime_
  pDescription_
  pPricingPlan_
  pTrackerName_
  pUpdateTime_ =
    ListTrackersResponseEntry'
      { pricingPlanDataSource =
          Prelude.Nothing,
        createTime = Core._Time Lens.# pCreateTime_,
        description = pDescription_,
        pricingPlan = pPricingPlan_,
        trackerName = pTrackerName_,
        updateTime = Core._Time Lens.# pUpdateTime_
      }

-- | The specified data provider for the tracker resource.
listTrackersResponseEntry_pricingPlanDataSource :: Lens.Lens' ListTrackersResponseEntry (Prelude.Maybe Prelude.Text)
listTrackersResponseEntry_pricingPlanDataSource = Lens.lens (\ListTrackersResponseEntry' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@ListTrackersResponseEntry' {} a -> s {pricingPlanDataSource = a} :: ListTrackersResponseEntry)

-- | The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listTrackersResponseEntry_createTime :: Lens.Lens' ListTrackersResponseEntry Prelude.UTCTime
listTrackersResponseEntry_createTime = Lens.lens (\ListTrackersResponseEntry' {createTime} -> createTime) (\s@ListTrackersResponseEntry' {} a -> s {createTime = a} :: ListTrackersResponseEntry) Prelude.. Core._Time

-- | The description for the tracker resource.
listTrackersResponseEntry_description :: Lens.Lens' ListTrackersResponseEntry Prelude.Text
listTrackersResponseEntry_description = Lens.lens (\ListTrackersResponseEntry' {description} -> description) (\s@ListTrackersResponseEntry' {} a -> s {description = a} :: ListTrackersResponseEntry)

-- | The pricing plan for the specified tracker resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
listTrackersResponseEntry_pricingPlan :: Lens.Lens' ListTrackersResponseEntry PricingPlan
listTrackersResponseEntry_pricingPlan = Lens.lens (\ListTrackersResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListTrackersResponseEntry' {} a -> s {pricingPlan = a} :: ListTrackersResponseEntry)

-- | The name of the tracker resource.
listTrackersResponseEntry_trackerName :: Lens.Lens' ListTrackersResponseEntry Prelude.Text
listTrackersResponseEntry_trackerName = Lens.lens (\ListTrackersResponseEntry' {trackerName} -> trackerName) (\s@ListTrackersResponseEntry' {} a -> s {trackerName = a} :: ListTrackersResponseEntry)

-- | The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listTrackersResponseEntry_updateTime :: Lens.Lens' ListTrackersResponseEntry Prelude.UTCTime
listTrackersResponseEntry_updateTime = Lens.lens (\ListTrackersResponseEntry' {updateTime} -> updateTime) (\s@ListTrackersResponseEntry' {} a -> s {updateTime = a} :: ListTrackersResponseEntry) Prelude.. Core._Time

instance Core.FromJSON ListTrackersResponseEntry where
  parseJSON =
    Core.withObject
      "ListTrackersResponseEntry"
      ( \x ->
          ListTrackersResponseEntry'
            Prelude.<$> (x Core..:? "PricingPlanDataSource")
            Prelude.<*> (x Core..: "CreateTime")
            Prelude.<*> (x Core..: "Description")
            Prelude.<*> (x Core..: "PricingPlan")
            Prelude.<*> (x Core..: "TrackerName")
            Prelude.<*> (x Core..: "UpdateTime")
      )

instance Prelude.Hashable ListTrackersResponseEntry where
  hashWithSalt salt' ListTrackersResponseEntry' {..} =
    salt' `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` trackerName
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` pricingPlanDataSource

instance Prelude.NFData ListTrackersResponseEntry where
  rnf ListTrackersResponseEntry' {..} =
    Prelude.rnf pricingPlanDataSource
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf trackerName
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createTime
