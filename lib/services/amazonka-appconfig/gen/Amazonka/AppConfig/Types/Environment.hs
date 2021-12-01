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
-- Module      : Amazonka.AppConfig.Types.Environment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Environment where

import Amazonka.AppConfig.Types.EnvironmentState
import Amazonka.AppConfig.Types.Monitor
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | The state of the environment. An environment can be in one of the
    -- following states: @READY_FOR_DEPLOYMENT@, @DEPLOYING@, @ROLLING_BACK@,
    -- or @ROLLED_BACK@
    state :: Prelude.Maybe EnvironmentState,
    -- | Amazon CloudWatch alarms monitored during the deployment.
    monitors :: Prelude.Maybe [Monitor],
    -- | The application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The environment ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the environment.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'environment_state' - The state of the environment. An environment can be in one of the
-- following states: @READY_FOR_DEPLOYMENT@, @DEPLOYING@, @ROLLING_BACK@,
-- or @ROLLED_BACK@
--
-- 'monitors', 'environment_monitors' - Amazon CloudWatch alarms monitored during the deployment.
--
-- 'applicationId', 'environment_applicationId' - The application ID.
--
-- 'name', 'environment_name' - The name of the environment.
--
-- 'id', 'environment_id' - The environment ID.
--
-- 'description', 'environment_description' - The description of the environment.
newEnvironment ::
  Environment
newEnvironment =
  Environment'
    { state = Prelude.Nothing,
      monitors = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The state of the environment. An environment can be in one of the
-- following states: @READY_FOR_DEPLOYMENT@, @DEPLOYING@, @ROLLING_BACK@,
-- or @ROLLED_BACK@
environment_state :: Lens.Lens' Environment (Prelude.Maybe EnvironmentState)
environment_state = Lens.lens (\Environment' {state} -> state) (\s@Environment' {} a -> s {state = a} :: Environment)

-- | Amazon CloudWatch alarms monitored during the deployment.
environment_monitors :: Lens.Lens' Environment (Prelude.Maybe [Monitor])
environment_monitors = Lens.lens (\Environment' {monitors} -> monitors) (\s@Environment' {} a -> s {monitors = a} :: Environment) Prelude.. Lens.mapping Lens.coerced

-- | The application ID.
environment_applicationId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_applicationId = Lens.lens (\Environment' {applicationId} -> applicationId) (\s@Environment' {} a -> s {applicationId = a} :: Environment)

-- | The name of the environment.
environment_name :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The environment ID.
environment_id :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_id = Lens.lens (\Environment' {id} -> id) (\s@Environment' {} a -> s {id = a} :: Environment)

-- | The description of the environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment)

instance Core.FromJSON Environment where
  parseJSON =
    Core.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Monitors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable Environment where
  hashWithSalt salt' Environment' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` monitors
      `Prelude.hashWithSalt` state

instance Prelude.NFData Environment where
  rnf Environment' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf monitors
