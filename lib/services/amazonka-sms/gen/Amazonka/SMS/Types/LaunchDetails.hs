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
-- Module      : Amazonka.SMS.Types.LaunchDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.LaunchDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the latest launch of an application.
--
-- /See:/ 'newLaunchDetails' smart constructor.
data LaunchDetails = LaunchDetails'
  { -- | The ID of the latest stack launched for this application.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The latest time that this application was launched successfully.
    latestLaunchTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the latest stack launched for this application.
    stackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'launchDetails_stackId' - The ID of the latest stack launched for this application.
--
-- 'latestLaunchTime', 'launchDetails_latestLaunchTime' - The latest time that this application was launched successfully.
--
-- 'stackName', 'launchDetails_stackName' - The name of the latest stack launched for this application.
newLaunchDetails ::
  LaunchDetails
newLaunchDetails =
  LaunchDetails'
    { stackId = Prelude.Nothing,
      latestLaunchTime = Prelude.Nothing,
      stackName = Prelude.Nothing
    }

-- | The ID of the latest stack launched for this application.
launchDetails_stackId :: Lens.Lens' LaunchDetails (Prelude.Maybe Prelude.Text)
launchDetails_stackId = Lens.lens (\LaunchDetails' {stackId} -> stackId) (\s@LaunchDetails' {} a -> s {stackId = a} :: LaunchDetails)

-- | The latest time that this application was launched successfully.
launchDetails_latestLaunchTime :: Lens.Lens' LaunchDetails (Prelude.Maybe Prelude.UTCTime)
launchDetails_latestLaunchTime = Lens.lens (\LaunchDetails' {latestLaunchTime} -> latestLaunchTime) (\s@LaunchDetails' {} a -> s {latestLaunchTime = a} :: LaunchDetails) Prelude.. Lens.mapping Core._Time

-- | The name of the latest stack launched for this application.
launchDetails_stackName :: Lens.Lens' LaunchDetails (Prelude.Maybe Prelude.Text)
launchDetails_stackName = Lens.lens (\LaunchDetails' {stackName} -> stackName) (\s@LaunchDetails' {} a -> s {stackName = a} :: LaunchDetails)

instance Core.FromJSON LaunchDetails where
  parseJSON =
    Core.withObject
      "LaunchDetails"
      ( \x ->
          LaunchDetails'
            Prelude.<$> (x Core..:? "stackId")
            Prelude.<*> (x Core..:? "latestLaunchTime")
            Prelude.<*> (x Core..:? "stackName")
      )

instance Prelude.Hashable LaunchDetails where
  hashWithSalt salt' LaunchDetails' {..} =
    salt' `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` latestLaunchTime
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData LaunchDetails where
  rnf LaunchDetails' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf latestLaunchTime
