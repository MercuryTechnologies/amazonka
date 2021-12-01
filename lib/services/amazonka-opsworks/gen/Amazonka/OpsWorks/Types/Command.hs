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
-- Module      : Amazonka.OpsWorks.Types.Command
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.Command where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a command.
--
-- /See:/ 'newCommand' smart constructor.
data Command = Command'
  { -- | The command deployment ID.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance where the command was executed.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The command status:
    --
    -- -   failed
    --
    -- -   successful
    --
    -- -   skipped
    --
    -- -   pending
    status :: Prelude.Maybe Prelude.Text,
    -- | The URL of the command log.
    logUrl :: Prelude.Maybe Prelude.Text,
    -- | Date and time when the command was run.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The command ID.
    commandId :: Prelude.Maybe Prelude.Text,
    -- | The command exit code.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The command type:
    --
    -- -   @configure@
    --
    -- -   @deploy@
    --
    -- -   @execute_recipes@
    --
    -- -   @install_dependencies@
    --
    -- -   @restart@
    --
    -- -   @rollback@
    --
    -- -   @setup@
    --
    -- -   @start@
    --
    -- -   @stop@
    --
    -- -   @undeploy@
    --
    -- -   @update_custom_cookbooks@
    --
    -- -   @update_dependencies@
    type' :: Prelude.Maybe Prelude.Text,
    -- | Date when the command completed.
    completedAt :: Prelude.Maybe Prelude.Text,
    -- | Date and time when the command was acknowledged.
    acknowledgedAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Command' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'command_deploymentId' - The command deployment ID.
--
-- 'instanceId', 'command_instanceId' - The ID of the instance where the command was executed.
--
-- 'status', 'command_status' - The command status:
--
-- -   failed
--
-- -   successful
--
-- -   skipped
--
-- -   pending
--
-- 'logUrl', 'command_logUrl' - The URL of the command log.
--
-- 'createdAt', 'command_createdAt' - Date and time when the command was run.
--
-- 'commandId', 'command_commandId' - The command ID.
--
-- 'exitCode', 'command_exitCode' - The command exit code.
--
-- 'type'', 'command_type' - The command type:
--
-- -   @configure@
--
-- -   @deploy@
--
-- -   @execute_recipes@
--
-- -   @install_dependencies@
--
-- -   @restart@
--
-- -   @rollback@
--
-- -   @setup@
--
-- -   @start@
--
-- -   @stop@
--
-- -   @undeploy@
--
-- -   @update_custom_cookbooks@
--
-- -   @update_dependencies@
--
-- 'completedAt', 'command_completedAt' - Date when the command completed.
--
-- 'acknowledgedAt', 'command_acknowledgedAt' - Date and time when the command was acknowledged.
newCommand ::
  Command
newCommand =
  Command'
    { deploymentId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      status = Prelude.Nothing,
      logUrl = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      commandId = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      type' = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      acknowledgedAt = Prelude.Nothing
    }

-- | The command deployment ID.
command_deploymentId :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_deploymentId = Lens.lens (\Command' {deploymentId} -> deploymentId) (\s@Command' {} a -> s {deploymentId = a} :: Command)

-- | The ID of the instance where the command was executed.
command_instanceId :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_instanceId = Lens.lens (\Command' {instanceId} -> instanceId) (\s@Command' {} a -> s {instanceId = a} :: Command)

-- | The command status:
--
-- -   failed
--
-- -   successful
--
-- -   skipped
--
-- -   pending
command_status :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_status = Lens.lens (\Command' {status} -> status) (\s@Command' {} a -> s {status = a} :: Command)

-- | The URL of the command log.
command_logUrl :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_logUrl = Lens.lens (\Command' {logUrl} -> logUrl) (\s@Command' {} a -> s {logUrl = a} :: Command)

-- | Date and time when the command was run.
command_createdAt :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_createdAt = Lens.lens (\Command' {createdAt} -> createdAt) (\s@Command' {} a -> s {createdAt = a} :: Command)

-- | The command ID.
command_commandId :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_commandId = Lens.lens (\Command' {commandId} -> commandId) (\s@Command' {} a -> s {commandId = a} :: Command)

-- | The command exit code.
command_exitCode :: Lens.Lens' Command (Prelude.Maybe Prelude.Int)
command_exitCode = Lens.lens (\Command' {exitCode} -> exitCode) (\s@Command' {} a -> s {exitCode = a} :: Command)

-- | The command type:
--
-- -   @configure@
--
-- -   @deploy@
--
-- -   @execute_recipes@
--
-- -   @install_dependencies@
--
-- -   @restart@
--
-- -   @rollback@
--
-- -   @setup@
--
-- -   @start@
--
-- -   @stop@
--
-- -   @undeploy@
--
-- -   @update_custom_cookbooks@
--
-- -   @update_dependencies@
command_type :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_type = Lens.lens (\Command' {type'} -> type') (\s@Command' {} a -> s {type' = a} :: Command)

-- | Date when the command completed.
command_completedAt :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_completedAt = Lens.lens (\Command' {completedAt} -> completedAt) (\s@Command' {} a -> s {completedAt = a} :: Command)

-- | Date and time when the command was acknowledged.
command_acknowledgedAt :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_acknowledgedAt = Lens.lens (\Command' {acknowledgedAt} -> acknowledgedAt) (\s@Command' {} a -> s {acknowledgedAt = a} :: Command)

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject
      "Command"
      ( \x ->
          Command'
            Prelude.<$> (x Core..:? "DeploymentId")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LogUrl")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "CommandId")
            Prelude.<*> (x Core..:? "ExitCode")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "CompletedAt")
            Prelude.<*> (x Core..:? "AcknowledgedAt")
      )

instance Prelude.Hashable Command where
  hashWithSalt salt' Command' {..} =
    salt' `Prelude.hashWithSalt` acknowledgedAt
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` exitCode
      `Prelude.hashWithSalt` commandId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` logUrl
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData Command where
  rnf Command' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf acknowledgedAt
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf exitCode
      `Prelude.seq` Prelude.rnf commandId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf logUrl
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf instanceId
