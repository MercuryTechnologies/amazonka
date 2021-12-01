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
-- Module      : Amazonka.GameLift.Types.PlayerLatency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PlayerLatency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Regional latency information for a player, used when requesting a new
-- game session with StartGameSessionPlacement. This value indicates the
-- amount of time lag that exists when the player is connected to a fleet
-- in the specified Region. The relative difference between a player\'s
-- latency values for multiple Regions are used to determine which fleets
-- are best suited to place a new game session for the player.
--
-- /See:/ 'newPlayerLatency' smart constructor.
data PlayerLatency = PlayerLatency'
  { -- | Amount of time that represents the time lag experienced by the player
    -- when connected to the specified Region.
    latencyInMilliseconds :: Prelude.Maybe Prelude.Double,
    -- | Name of the Region that is associated with the latency value.
    regionIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player associated with the latency data.
    playerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlayerLatency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latencyInMilliseconds', 'playerLatency_latencyInMilliseconds' - Amount of time that represents the time lag experienced by the player
-- when connected to the specified Region.
--
-- 'regionIdentifier', 'playerLatency_regionIdentifier' - Name of the Region that is associated with the latency value.
--
-- 'playerId', 'playerLatency_playerId' - A unique identifier for a player associated with the latency data.
newPlayerLatency ::
  PlayerLatency
newPlayerLatency =
  PlayerLatency'
    { latencyInMilliseconds =
        Prelude.Nothing,
      regionIdentifier = Prelude.Nothing,
      playerId = Prelude.Nothing
    }

-- | Amount of time that represents the time lag experienced by the player
-- when connected to the specified Region.
playerLatency_latencyInMilliseconds :: Lens.Lens' PlayerLatency (Prelude.Maybe Prelude.Double)
playerLatency_latencyInMilliseconds = Lens.lens (\PlayerLatency' {latencyInMilliseconds} -> latencyInMilliseconds) (\s@PlayerLatency' {} a -> s {latencyInMilliseconds = a} :: PlayerLatency)

-- | Name of the Region that is associated with the latency value.
playerLatency_regionIdentifier :: Lens.Lens' PlayerLatency (Prelude.Maybe Prelude.Text)
playerLatency_regionIdentifier = Lens.lens (\PlayerLatency' {regionIdentifier} -> regionIdentifier) (\s@PlayerLatency' {} a -> s {regionIdentifier = a} :: PlayerLatency)

-- | A unique identifier for a player associated with the latency data.
playerLatency_playerId :: Lens.Lens' PlayerLatency (Prelude.Maybe Prelude.Text)
playerLatency_playerId = Lens.lens (\PlayerLatency' {playerId} -> playerId) (\s@PlayerLatency' {} a -> s {playerId = a} :: PlayerLatency)

instance Core.FromJSON PlayerLatency where
  parseJSON =
    Core.withObject
      "PlayerLatency"
      ( \x ->
          PlayerLatency'
            Prelude.<$> (x Core..:? "LatencyInMilliseconds")
            Prelude.<*> (x Core..:? "RegionIdentifier")
            Prelude.<*> (x Core..:? "PlayerId")
      )

instance Prelude.Hashable PlayerLatency where
  hashWithSalt salt' PlayerLatency' {..} =
    salt' `Prelude.hashWithSalt` playerId
      `Prelude.hashWithSalt` regionIdentifier
      `Prelude.hashWithSalt` latencyInMilliseconds

instance Prelude.NFData PlayerLatency where
  rnf PlayerLatency' {..} =
    Prelude.rnf latencyInMilliseconds
      `Prelude.seq` Prelude.rnf playerId
      `Prelude.seq` Prelude.rnf regionIdentifier

instance Core.ToJSON PlayerLatency where
  toJSON PlayerLatency' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LatencyInMilliseconds" Core..=)
              Prelude.<$> latencyInMilliseconds,
            ("RegionIdentifier" Core..=)
              Prelude.<$> regionIdentifier,
            ("PlayerId" Core..=) Prelude.<$> playerId
          ]
      )
