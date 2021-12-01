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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.MaintenanceWindowResourceType
import Amazonka.SSM.Types.Target

-- | The target registered with the maintenance window.
--
-- /See:/ 'newMaintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
  { -- | The type of target that is being registered with the maintenance window.
    resourceType :: Prelude.Maybe MaintenanceWindowResourceType,
    -- | A user-provided value that will be included in any Amazon CloudWatch
    -- Events events that are raised while running tasks for these targets in
    -- this maintenance window.
    ownerInformation :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the target.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The name for the maintenance window target.
    name :: Prelude.Maybe Prelude.Text,
    -- | The targets, either instances or tags.
    --
    -- Specify instances using the following format:
    --
    -- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
    --
    -- Tags are specified using the following format:
    --
    -- @Key=\<tag name>,Values=\<tag value>@.
    targets :: Prelude.Maybe [Target],
    -- | A description for the target.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the maintenance window to register the target with.
    windowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'maintenanceWindowTarget_resourceType' - The type of target that is being registered with the maintenance window.
--
-- 'ownerInformation', 'maintenanceWindowTarget_ownerInformation' - A user-provided value that will be included in any Amazon CloudWatch
-- Events events that are raised while running tasks for these targets in
-- this maintenance window.
--
-- 'windowTargetId', 'maintenanceWindowTarget_windowTargetId' - The ID of the target.
--
-- 'name', 'maintenanceWindowTarget_name' - The name for the maintenance window target.
--
-- 'targets', 'maintenanceWindowTarget_targets' - The targets, either instances or tags.
--
-- Specify instances using the following format:
--
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
--
-- Tags are specified using the following format:
--
-- @Key=\<tag name>,Values=\<tag value>@.
--
-- 'description', 'maintenanceWindowTarget_description' - A description for the target.
--
-- 'windowId', 'maintenanceWindowTarget_windowId' - The ID of the maintenance window to register the target with.
newMaintenanceWindowTarget ::
  MaintenanceWindowTarget
newMaintenanceWindowTarget =
  MaintenanceWindowTarget'
    { resourceType =
        Prelude.Nothing,
      ownerInformation = Prelude.Nothing,
      windowTargetId = Prelude.Nothing,
      name = Prelude.Nothing,
      targets = Prelude.Nothing,
      description = Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The type of target that is being registered with the maintenance window.
maintenanceWindowTarget_resourceType :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe MaintenanceWindowResourceType)
maintenanceWindowTarget_resourceType = Lens.lens (\MaintenanceWindowTarget' {resourceType} -> resourceType) (\s@MaintenanceWindowTarget' {} a -> s {resourceType = a} :: MaintenanceWindowTarget)

-- | A user-provided value that will be included in any Amazon CloudWatch
-- Events events that are raised while running tasks for these targets in
-- this maintenance window.
maintenanceWindowTarget_ownerInformation :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_ownerInformation = Lens.lens (\MaintenanceWindowTarget' {ownerInformation} -> ownerInformation) (\s@MaintenanceWindowTarget' {} a -> s {ownerInformation = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the target.
maintenanceWindowTarget_windowTargetId :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_windowTargetId = Lens.lens (\MaintenanceWindowTarget' {windowTargetId} -> windowTargetId) (\s@MaintenanceWindowTarget' {} a -> s {windowTargetId = a} :: MaintenanceWindowTarget)

-- | The name for the maintenance window target.
maintenanceWindowTarget_name :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_name = Lens.lens (\MaintenanceWindowTarget' {name} -> name) (\s@MaintenanceWindowTarget' {} a -> s {name = a} :: MaintenanceWindowTarget)

-- | The targets, either instances or tags.
--
-- Specify instances using the following format:
--
-- @Key=instanceids,Values=\<instanceid1>,\<instanceid2>@
--
-- Tags are specified using the following format:
--
-- @Key=\<tag name>,Values=\<tag value>@.
maintenanceWindowTarget_targets :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe [Target])
maintenanceWindowTarget_targets = Lens.lens (\MaintenanceWindowTarget' {targets} -> targets) (\s@MaintenanceWindowTarget' {} a -> s {targets = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Lens.coerced

-- | A description for the target.
maintenanceWindowTarget_description :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_description = Lens.lens (\MaintenanceWindowTarget' {description} -> description) (\s@MaintenanceWindowTarget' {} a -> s {description = a} :: MaintenanceWindowTarget) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the maintenance window to register the target with.
maintenanceWindowTarget_windowId :: Lens.Lens' MaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowTarget_windowId = Lens.lens (\MaintenanceWindowTarget' {windowId} -> windowId) (\s@MaintenanceWindowTarget' {} a -> s {windowId = a} :: MaintenanceWindowTarget)

instance Core.FromJSON MaintenanceWindowTarget where
  parseJSON =
    Core.withObject
      "MaintenanceWindowTarget"
      ( \x ->
          MaintenanceWindowTarget'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "OwnerInformation")
            Prelude.<*> (x Core..:? "WindowTargetId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "WindowId")
      )

instance Prelude.Hashable MaintenanceWindowTarget where
  hashWithSalt salt' MaintenanceWindowTarget' {..} =
    salt' `Prelude.hashWithSalt` windowId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` windowTargetId
      `Prelude.hashWithSalt` ownerInformation
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData MaintenanceWindowTarget where
  rnf MaintenanceWindowTarget' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf windowTargetId
      `Prelude.seq` Prelude.rnf ownerInformation
