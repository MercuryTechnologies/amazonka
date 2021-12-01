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
-- Module      : Amazonka.MigrationHubConfig.Types.HomeRegionControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubConfig.Types.HomeRegionControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MigrationHubConfig.Types.Target
import qualified Amazonka.Prelude as Prelude

-- | A home region control is an object that specifies the home region for an
-- account, with some additional information. It contains a target (always
-- of type @ACCOUNT@), an ID, and a time at which the home region was set.
--
-- /See:/ 'newHomeRegionControl' smart constructor.
data HomeRegionControl = HomeRegionControl'
  { -- | A timestamp representing the time when the customer called
    -- @CreateHomeregionControl@ and set the home region for the account.
    requestedTime :: Prelude.Maybe Core.POSIX,
    -- | The AWS Region that\'s been set as home region. For example,
    -- \"us-west-2\" or \"eu-central-1\" are valid home regions.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that\'s generated for each home region control.
    -- It\'s always a string that begins with \"hrc-\" followed by 12 lowercase
    -- letters and numbers.
    controlId :: Prelude.Maybe Prelude.Text,
    -- | The target parameter specifies the identifier to which the home region
    -- is applied, which is always an @ACCOUNT@. It applies the home region to
    -- the current @ACCOUNT@.
    target :: Prelude.Maybe Target
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HomeRegionControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestedTime', 'homeRegionControl_requestedTime' - A timestamp representing the time when the customer called
-- @CreateHomeregionControl@ and set the home region for the account.
--
-- 'homeRegion', 'homeRegionControl_homeRegion' - The AWS Region that\'s been set as home region. For example,
-- \"us-west-2\" or \"eu-central-1\" are valid home regions.
--
-- 'controlId', 'homeRegionControl_controlId' - A unique identifier that\'s generated for each home region control.
-- It\'s always a string that begins with \"hrc-\" followed by 12 lowercase
-- letters and numbers.
--
-- 'target', 'homeRegionControl_target' - The target parameter specifies the identifier to which the home region
-- is applied, which is always an @ACCOUNT@. It applies the home region to
-- the current @ACCOUNT@.
newHomeRegionControl ::
  HomeRegionControl
newHomeRegionControl =
  HomeRegionControl'
    { requestedTime = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      controlId = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | A timestamp representing the time when the customer called
-- @CreateHomeregionControl@ and set the home region for the account.
homeRegionControl_requestedTime :: Lens.Lens' HomeRegionControl (Prelude.Maybe Prelude.UTCTime)
homeRegionControl_requestedTime = Lens.lens (\HomeRegionControl' {requestedTime} -> requestedTime) (\s@HomeRegionControl' {} a -> s {requestedTime = a} :: HomeRegionControl) Prelude.. Lens.mapping Core._Time

-- | The AWS Region that\'s been set as home region. For example,
-- \"us-west-2\" or \"eu-central-1\" are valid home regions.
homeRegionControl_homeRegion :: Lens.Lens' HomeRegionControl (Prelude.Maybe Prelude.Text)
homeRegionControl_homeRegion = Lens.lens (\HomeRegionControl' {homeRegion} -> homeRegion) (\s@HomeRegionControl' {} a -> s {homeRegion = a} :: HomeRegionControl)

-- | A unique identifier that\'s generated for each home region control.
-- It\'s always a string that begins with \"hrc-\" followed by 12 lowercase
-- letters and numbers.
homeRegionControl_controlId :: Lens.Lens' HomeRegionControl (Prelude.Maybe Prelude.Text)
homeRegionControl_controlId = Lens.lens (\HomeRegionControl' {controlId} -> controlId) (\s@HomeRegionControl' {} a -> s {controlId = a} :: HomeRegionControl)

-- | The target parameter specifies the identifier to which the home region
-- is applied, which is always an @ACCOUNT@. It applies the home region to
-- the current @ACCOUNT@.
homeRegionControl_target :: Lens.Lens' HomeRegionControl (Prelude.Maybe Target)
homeRegionControl_target = Lens.lens (\HomeRegionControl' {target} -> target) (\s@HomeRegionControl' {} a -> s {target = a} :: HomeRegionControl)

instance Core.FromJSON HomeRegionControl where
  parseJSON =
    Core.withObject
      "HomeRegionControl"
      ( \x ->
          HomeRegionControl'
            Prelude.<$> (x Core..:? "RequestedTime")
            Prelude.<*> (x Core..:? "HomeRegion")
            Prelude.<*> (x Core..:? "ControlId")
            Prelude.<*> (x Core..:? "Target")
      )

instance Prelude.Hashable HomeRegionControl where
  hashWithSalt salt' HomeRegionControl' {..} =
    salt' `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` controlId
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` requestedTime

instance Prelude.NFData HomeRegionControl where
  rnf HomeRegionControl' {..} =
    Prelude.rnf requestedTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf controlId
      `Prelude.seq` Prelude.rnf homeRegion
