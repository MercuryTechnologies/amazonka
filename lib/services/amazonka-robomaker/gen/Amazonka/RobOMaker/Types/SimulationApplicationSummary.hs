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
-- Module      : Amazonka.RobOMaker.Types.SimulationApplicationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.RobotSoftwareSuite
import Amazonka.RobOMaker.Types.SimulationSoftwareSuite

-- | Summary information for a simulation application.
--
-- /See:/ 'newSimulationApplicationSummary' smart constructor.
data SimulationApplicationSummary = SimulationApplicationSummary'
  { -- | The time, in milliseconds since the epoch, when the simulation
    -- application was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the simulation application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the simulation application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the simulation application.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information about a simulation software suite.
    simulationSoftwareSuite :: Prelude.Maybe SimulationSoftwareSuite,
    -- | Information about a robot software suite (ROS distribution).
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedAt', 'simulationApplicationSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
--
-- 'arn', 'simulationApplicationSummary_arn' - The Amazon Resource Name (ARN) of the simulation application.
--
-- 'name', 'simulationApplicationSummary_name' - The name of the simulation application.
--
-- 'version', 'simulationApplicationSummary_version' - The version of the simulation application.
--
-- 'simulationSoftwareSuite', 'simulationApplicationSummary_simulationSoftwareSuite' - Information about a simulation software suite.
--
-- 'robotSoftwareSuite', 'simulationApplicationSummary_robotSoftwareSuite' - Information about a robot software suite (ROS distribution).
newSimulationApplicationSummary ::
  SimulationApplicationSummary
newSimulationApplicationSummary =
  SimulationApplicationSummary'
    { lastUpdatedAt =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      simulationSoftwareSuite = Prelude.Nothing,
      robotSoftwareSuite = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
simulationApplicationSummary_lastUpdatedAt :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.UTCTime)
simulationApplicationSummary_lastUpdatedAt = Lens.lens (\SimulationApplicationSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@SimulationApplicationSummary' {} a -> s {lastUpdatedAt = a} :: SimulationApplicationSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the simulation application.
simulationApplicationSummary_arn :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.Text)
simulationApplicationSummary_arn = Lens.lens (\SimulationApplicationSummary' {arn} -> arn) (\s@SimulationApplicationSummary' {} a -> s {arn = a} :: SimulationApplicationSummary)

-- | The name of the simulation application.
simulationApplicationSummary_name :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.Text)
simulationApplicationSummary_name = Lens.lens (\SimulationApplicationSummary' {name} -> name) (\s@SimulationApplicationSummary' {} a -> s {name = a} :: SimulationApplicationSummary)

-- | The version of the simulation application.
simulationApplicationSummary_version :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe Prelude.Text)
simulationApplicationSummary_version = Lens.lens (\SimulationApplicationSummary' {version} -> version) (\s@SimulationApplicationSummary' {} a -> s {version = a} :: SimulationApplicationSummary)

-- | Information about a simulation software suite.
simulationApplicationSummary_simulationSoftwareSuite :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe SimulationSoftwareSuite)
simulationApplicationSummary_simulationSoftwareSuite = Lens.lens (\SimulationApplicationSummary' {simulationSoftwareSuite} -> simulationSoftwareSuite) (\s@SimulationApplicationSummary' {} a -> s {simulationSoftwareSuite = a} :: SimulationApplicationSummary)

-- | Information about a robot software suite (ROS distribution).
simulationApplicationSummary_robotSoftwareSuite :: Lens.Lens' SimulationApplicationSummary (Prelude.Maybe RobotSoftwareSuite)
simulationApplicationSummary_robotSoftwareSuite = Lens.lens (\SimulationApplicationSummary' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@SimulationApplicationSummary' {} a -> s {robotSoftwareSuite = a} :: SimulationApplicationSummary)

instance Core.FromJSON SimulationApplicationSummary where
  parseJSON =
    Core.withObject
      "SimulationApplicationSummary"
      ( \x ->
          SimulationApplicationSummary'
            Prelude.<$> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "simulationSoftwareSuite")
            Prelude.<*> (x Core..:? "robotSoftwareSuite")
      )

instance
  Prelude.Hashable
    SimulationApplicationSummary
  where
  hashWithSalt salt' SimulationApplicationSummary' {..} =
    salt' `Prelude.hashWithSalt` robotSoftwareSuite
      `Prelude.hashWithSalt` simulationSoftwareSuite
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedAt

instance Prelude.NFData SimulationApplicationSummary where
  rnf SimulationApplicationSummary' {..} =
    Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf simulationSoftwareSuite
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
