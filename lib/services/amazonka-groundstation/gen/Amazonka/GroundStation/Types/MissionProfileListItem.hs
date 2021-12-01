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
-- Module      : Amazonka.GroundStation.Types.MissionProfileListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.MissionProfileListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Item in a list of mission profiles.
--
-- /See:/ 'newMissionProfileListItem' smart constructor.
data MissionProfileListItem = MissionProfileListItem'
  { -- | UUID of a mission profile.
    missionProfileId :: Prelude.Maybe Prelude.Text,
    -- | ARN of a mission profile.
    missionProfileArn :: Prelude.Maybe Prelude.Text,
    -- | Name of a mission profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | Region of a mission profile.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MissionProfileListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missionProfileId', 'missionProfileListItem_missionProfileId' - UUID of a mission profile.
--
-- 'missionProfileArn', 'missionProfileListItem_missionProfileArn' - ARN of a mission profile.
--
-- 'name', 'missionProfileListItem_name' - Name of a mission profile.
--
-- 'region', 'missionProfileListItem_region' - Region of a mission profile.
newMissionProfileListItem ::
  MissionProfileListItem
newMissionProfileListItem =
  MissionProfileListItem'
    { missionProfileId =
        Prelude.Nothing,
      missionProfileArn = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | UUID of a mission profile.
missionProfileListItem_missionProfileId :: Lens.Lens' MissionProfileListItem (Prelude.Maybe Prelude.Text)
missionProfileListItem_missionProfileId = Lens.lens (\MissionProfileListItem' {missionProfileId} -> missionProfileId) (\s@MissionProfileListItem' {} a -> s {missionProfileId = a} :: MissionProfileListItem)

-- | ARN of a mission profile.
missionProfileListItem_missionProfileArn :: Lens.Lens' MissionProfileListItem (Prelude.Maybe Prelude.Text)
missionProfileListItem_missionProfileArn = Lens.lens (\MissionProfileListItem' {missionProfileArn} -> missionProfileArn) (\s@MissionProfileListItem' {} a -> s {missionProfileArn = a} :: MissionProfileListItem)

-- | Name of a mission profile.
missionProfileListItem_name :: Lens.Lens' MissionProfileListItem (Prelude.Maybe Prelude.Text)
missionProfileListItem_name = Lens.lens (\MissionProfileListItem' {name} -> name) (\s@MissionProfileListItem' {} a -> s {name = a} :: MissionProfileListItem)

-- | Region of a mission profile.
missionProfileListItem_region :: Lens.Lens' MissionProfileListItem (Prelude.Maybe Prelude.Text)
missionProfileListItem_region = Lens.lens (\MissionProfileListItem' {region} -> region) (\s@MissionProfileListItem' {} a -> s {region = a} :: MissionProfileListItem)

instance Core.FromJSON MissionProfileListItem where
  parseJSON =
    Core.withObject
      "MissionProfileListItem"
      ( \x ->
          MissionProfileListItem'
            Prelude.<$> (x Core..:? "missionProfileId")
            Prelude.<*> (x Core..:? "missionProfileArn")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "region")
      )

instance Prelude.Hashable MissionProfileListItem where
  hashWithSalt salt' MissionProfileListItem' {..} =
    salt' `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` missionProfileArn
      `Prelude.hashWithSalt` missionProfileId

instance Prelude.NFData MissionProfileListItem where
  rnf MissionProfileListItem' {..} =
    Prelude.rnf missionProfileId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf missionProfileArn
