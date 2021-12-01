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
-- Module      : Amazonka.AppSync.Types.SyncConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.SyncConfig where

import Amazonka.AppSync.Types.ConflictDetectionType
import Amazonka.AppSync.Types.ConflictHandlerType
import Amazonka.AppSync.Types.LambdaConflictHandlerConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a Sync configuration for a resolver.
--
-- Contains information on which Conflict Detection as well as Resolution
-- strategy should be performed when the resolver is invoked.
--
-- /See:/ 'newSyncConfig' smart constructor.
data SyncConfig = SyncConfig'
  { -- | The Conflict Resolution strategy to perform in the event of a conflict.
    --
    -- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
    --     when versions do not match the latest version at the server.
    --
    -- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
    --     resolution strategy.
    --
    -- -   __LAMBDA__: Resolve conflicts with a Lambda function supplied in the
    --     LambdaConflictHandlerConfig.
    conflictHandler :: Prelude.Maybe ConflictHandlerType,
    -- | The Conflict Detection strategy to use.
    --
    -- -   __VERSION__: Detect conflicts based on object versions for this
    --     resolver.
    --
    -- -   __NONE__: Do not detect conflicts when executing this resolver.
    conflictDetection :: Prelude.Maybe ConflictDetectionType,
    -- | The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the
    -- Conflict Handler.
    lambdaConflictHandlerConfig :: Prelude.Maybe LambdaConflictHandlerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictHandler', 'syncConfig_conflictHandler' - The Conflict Resolution strategy to perform in the event of a conflict.
--
-- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
--     when versions do not match the latest version at the server.
--
-- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
--     resolution strategy.
--
-- -   __LAMBDA__: Resolve conflicts with a Lambda function supplied in the
--     LambdaConflictHandlerConfig.
--
-- 'conflictDetection', 'syncConfig_conflictDetection' - The Conflict Detection strategy to use.
--
-- -   __VERSION__: Detect conflicts based on object versions for this
--     resolver.
--
-- -   __NONE__: Do not detect conflicts when executing this resolver.
--
-- 'lambdaConflictHandlerConfig', 'syncConfig_lambdaConflictHandlerConfig' - The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the
-- Conflict Handler.
newSyncConfig ::
  SyncConfig
newSyncConfig =
  SyncConfig'
    { conflictHandler = Prelude.Nothing,
      conflictDetection = Prelude.Nothing,
      lambdaConflictHandlerConfig = Prelude.Nothing
    }

-- | The Conflict Resolution strategy to perform in the event of a conflict.
--
-- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
--     when versions do not match the latest version at the server.
--
-- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
--     resolution strategy.
--
-- -   __LAMBDA__: Resolve conflicts with a Lambda function supplied in the
--     LambdaConflictHandlerConfig.
syncConfig_conflictHandler :: Lens.Lens' SyncConfig (Prelude.Maybe ConflictHandlerType)
syncConfig_conflictHandler = Lens.lens (\SyncConfig' {conflictHandler} -> conflictHandler) (\s@SyncConfig' {} a -> s {conflictHandler = a} :: SyncConfig)

-- | The Conflict Detection strategy to use.
--
-- -   __VERSION__: Detect conflicts based on object versions for this
--     resolver.
--
-- -   __NONE__: Do not detect conflicts when executing this resolver.
syncConfig_conflictDetection :: Lens.Lens' SyncConfig (Prelude.Maybe ConflictDetectionType)
syncConfig_conflictDetection = Lens.lens (\SyncConfig' {conflictDetection} -> conflictDetection) (\s@SyncConfig' {} a -> s {conflictDetection = a} :: SyncConfig)

-- | The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the
-- Conflict Handler.
syncConfig_lambdaConflictHandlerConfig :: Lens.Lens' SyncConfig (Prelude.Maybe LambdaConflictHandlerConfig)
syncConfig_lambdaConflictHandlerConfig = Lens.lens (\SyncConfig' {lambdaConflictHandlerConfig} -> lambdaConflictHandlerConfig) (\s@SyncConfig' {} a -> s {lambdaConflictHandlerConfig = a} :: SyncConfig)

instance Core.FromJSON SyncConfig where
  parseJSON =
    Core.withObject
      "SyncConfig"
      ( \x ->
          SyncConfig'
            Prelude.<$> (x Core..:? "conflictHandler")
            Prelude.<*> (x Core..:? "conflictDetection")
            Prelude.<*> (x Core..:? "lambdaConflictHandlerConfig")
      )

instance Prelude.Hashable SyncConfig where
  hashWithSalt salt' SyncConfig' {..} =
    salt'
      `Prelude.hashWithSalt` lambdaConflictHandlerConfig
      `Prelude.hashWithSalt` conflictDetection
      `Prelude.hashWithSalt` conflictHandler

instance Prelude.NFData SyncConfig where
  rnf SyncConfig' {..} =
    Prelude.rnf conflictHandler
      `Prelude.seq` Prelude.rnf lambdaConflictHandlerConfig
      `Prelude.seq` Prelude.rnf conflictDetection

instance Core.ToJSON SyncConfig where
  toJSON SyncConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("conflictHandler" Core..=)
              Prelude.<$> conflictHandler,
            ("conflictDetection" Core..=)
              Prelude.<$> conflictDetection,
            ("lambdaConflictHandlerConfig" Core..=)
              Prelude.<$> lambdaConflictHandlerConfig
          ]
      )
