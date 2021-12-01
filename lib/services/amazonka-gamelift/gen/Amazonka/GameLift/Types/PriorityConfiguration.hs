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
-- Module      : Amazonka.GameLift.Types.PriorityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PriorityConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types.PriorityType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Custom prioritization settings for use by a game session queue when
-- placing new game sessions with available game servers. When defined,
-- this configuration replaces the default FleetIQ prioritization process,
-- which is as follows:
--
-- -   If player latency data is included in a game session request,
--     destinations and locations are prioritized first based on lowest
--     average latency (1), then on lowest hosting cost (2), then on
--     destination list order (3), and finally on location (alphabetical)
--     (4). This approach ensures that the queue\'s top priority is to
--     place game sessions where average player latency is lowest, and--if
--     latency is the same--where the hosting cost is less, etc.
--
-- -   If player latency data is not included, destinations and locations
--     are prioritized first on destination list order (1), and then on
--     location (alphabetical) (2). This approach ensures that the queue\'s
--     top priority is to place game sessions on the first destination
--     fleet listed. If that fleet has multiple locations, the game session
--     is placed on the first location (when listed alphabetically).
--
-- Changing the priority order will affect how game sessions are placed.
--
-- Priority configurations are part of a GameSessionQueue.
--
-- /See:/ 'newPriorityConfiguration' smart constructor.
data PriorityConfiguration = PriorityConfiguration'
  { -- | The recommended sequence to use when prioritizing where to place new
    -- game sessions. Each type can only be listed once.
    --
    -- -   @LATENCY@ -- FleetIQ prioritizes locations where the average player
    --     latency (provided in each game session request) is lowest.
    --
    -- -   @COST@ -- FleetIQ prioritizes destinations with the lowest current
    --     hosting costs. Cost is evaluated based on the location, instance
    --     type, and fleet type (Spot or On-Demand) for each destination in the
    --     queue.
    --
    -- -   @DESTINATION@ -- FleetIQ prioritizes based on the order that
    --     destinations are listed in the queue configuration.
    --
    -- -   @LOCATION@ -- FleetIQ prioritizes based on the provided order of
    --     locations, as defined in @LocationOrder@.
    priorityOrder :: Prelude.Maybe (Prelude.NonEmpty PriorityType),
    -- | The prioritization order to use for fleet locations, when the
    -- @PriorityOrder@ property includes @LOCATION@. Locations are identified
    -- by AWS Region codes such as @us-west-2@. Each location can only be
    -- listed once.
    locationOrder :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PriorityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priorityOrder', 'priorityConfiguration_priorityOrder' - The recommended sequence to use when prioritizing where to place new
-- game sessions. Each type can only be listed once.
--
-- -   @LATENCY@ -- FleetIQ prioritizes locations where the average player
--     latency (provided in each game session request) is lowest.
--
-- -   @COST@ -- FleetIQ prioritizes destinations with the lowest current
--     hosting costs. Cost is evaluated based on the location, instance
--     type, and fleet type (Spot or On-Demand) for each destination in the
--     queue.
--
-- -   @DESTINATION@ -- FleetIQ prioritizes based on the order that
--     destinations are listed in the queue configuration.
--
-- -   @LOCATION@ -- FleetIQ prioritizes based on the provided order of
--     locations, as defined in @LocationOrder@.
--
-- 'locationOrder', 'priorityConfiguration_locationOrder' - The prioritization order to use for fleet locations, when the
-- @PriorityOrder@ property includes @LOCATION@. Locations are identified
-- by AWS Region codes such as @us-west-2@. Each location can only be
-- listed once.
newPriorityConfiguration ::
  PriorityConfiguration
newPriorityConfiguration =
  PriorityConfiguration'
    { priorityOrder =
        Prelude.Nothing,
      locationOrder = Prelude.Nothing
    }

-- | The recommended sequence to use when prioritizing where to place new
-- game sessions. Each type can only be listed once.
--
-- -   @LATENCY@ -- FleetIQ prioritizes locations where the average player
--     latency (provided in each game session request) is lowest.
--
-- -   @COST@ -- FleetIQ prioritizes destinations with the lowest current
--     hosting costs. Cost is evaluated based on the location, instance
--     type, and fleet type (Spot or On-Demand) for each destination in the
--     queue.
--
-- -   @DESTINATION@ -- FleetIQ prioritizes based on the order that
--     destinations are listed in the queue configuration.
--
-- -   @LOCATION@ -- FleetIQ prioritizes based on the provided order of
--     locations, as defined in @LocationOrder@.
priorityConfiguration_priorityOrder :: Lens.Lens' PriorityConfiguration (Prelude.Maybe (Prelude.NonEmpty PriorityType))
priorityConfiguration_priorityOrder = Lens.lens (\PriorityConfiguration' {priorityOrder} -> priorityOrder) (\s@PriorityConfiguration' {} a -> s {priorityOrder = a} :: PriorityConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The prioritization order to use for fleet locations, when the
-- @PriorityOrder@ property includes @LOCATION@. Locations are identified
-- by AWS Region codes such as @us-west-2@. Each location can only be
-- listed once.
priorityConfiguration_locationOrder :: Lens.Lens' PriorityConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
priorityConfiguration_locationOrder = Lens.lens (\PriorityConfiguration' {locationOrder} -> locationOrder) (\s@PriorityConfiguration' {} a -> s {locationOrder = a} :: PriorityConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PriorityConfiguration where
  parseJSON =
    Core.withObject
      "PriorityConfiguration"
      ( \x ->
          PriorityConfiguration'
            Prelude.<$> (x Core..:? "PriorityOrder")
            Prelude.<*> (x Core..:? "LocationOrder")
      )

instance Prelude.Hashable PriorityConfiguration where
  hashWithSalt salt' PriorityConfiguration' {..} =
    salt' `Prelude.hashWithSalt` locationOrder
      `Prelude.hashWithSalt` priorityOrder

instance Prelude.NFData PriorityConfiguration where
  rnf PriorityConfiguration' {..} =
    Prelude.rnf priorityOrder
      `Prelude.seq` Prelude.rnf locationOrder

instance Core.ToJSON PriorityConfiguration where
  toJSON PriorityConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PriorityOrder" Core..=) Prelude.<$> priorityOrder,
            ("LocationOrder" Core..=) Prelude.<$> locationOrder
          ]
      )
