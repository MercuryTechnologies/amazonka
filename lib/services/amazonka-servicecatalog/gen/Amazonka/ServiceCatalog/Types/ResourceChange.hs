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
-- Module      : Amazonka.ServiceCatalog.Types.ResourceChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ResourceChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ChangeAction
import Amazonka.ServiceCatalog.Types.Replacement
import Amazonka.ServiceCatalog.Types.ResourceAttribute
import Amazonka.ServiceCatalog.Types.ResourceChangeDetail

-- | Information about a resource change that will occur when a plan is
-- executed.
--
-- /See:/ 'newResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The ID of the resource, as defined in the CloudFormation template.
    logicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource, if it was already created.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The change action.
    action :: Prelude.Maybe ChangeAction,
    -- | The change scope.
    scope :: Prelude.Maybe [ResourceAttribute],
    -- | Information about the resource changes.
    details :: Prelude.Maybe [ResourceChangeDetail],
    -- | If the change type is @Modify@, indicates whether the existing resource
    -- is deleted and replaced with a new one.
    replacement :: Prelude.Maybe Replacement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalResourceId', 'resourceChange_logicalResourceId' - The ID of the resource, as defined in the CloudFormation template.
--
-- 'physicalResourceId', 'resourceChange_physicalResourceId' - The ID of the resource, if it was already created.
--
-- 'resourceType', 'resourceChange_resourceType' - The type of resource.
--
-- 'action', 'resourceChange_action' - The change action.
--
-- 'scope', 'resourceChange_scope' - The change scope.
--
-- 'details', 'resourceChange_details' - Information about the resource changes.
--
-- 'replacement', 'resourceChange_replacement' - If the change type is @Modify@, indicates whether the existing resource
-- is deleted and replaced with a new one.
newResourceChange ::
  ResourceChange
newResourceChange =
  ResourceChange'
    { logicalResourceId =
        Prelude.Nothing,
      physicalResourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      action = Prelude.Nothing,
      scope = Prelude.Nothing,
      details = Prelude.Nothing,
      replacement = Prelude.Nothing
    }

-- | The ID of the resource, as defined in the CloudFormation template.
resourceChange_logicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_logicalResourceId = Lens.lens (\ResourceChange' {logicalResourceId} -> logicalResourceId) (\s@ResourceChange' {} a -> s {logicalResourceId = a} :: ResourceChange)

-- | The ID of the resource, if it was already created.
resourceChange_physicalResourceId :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_physicalResourceId = Lens.lens (\ResourceChange' {physicalResourceId} -> physicalResourceId) (\s@ResourceChange' {} a -> s {physicalResourceId = a} :: ResourceChange)

-- | The type of resource.
resourceChange_resourceType :: Lens.Lens' ResourceChange (Prelude.Maybe Prelude.Text)
resourceChange_resourceType = Lens.lens (\ResourceChange' {resourceType} -> resourceType) (\s@ResourceChange' {} a -> s {resourceType = a} :: ResourceChange)

-- | The change action.
resourceChange_action :: Lens.Lens' ResourceChange (Prelude.Maybe ChangeAction)
resourceChange_action = Lens.lens (\ResourceChange' {action} -> action) (\s@ResourceChange' {} a -> s {action = a} :: ResourceChange)

-- | The change scope.
resourceChange_scope :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceAttribute])
resourceChange_scope = Lens.lens (\ResourceChange' {scope} -> scope) (\s@ResourceChange' {} a -> s {scope = a} :: ResourceChange) Prelude.. Lens.mapping Lens.coerced

-- | Information about the resource changes.
resourceChange_details :: Lens.Lens' ResourceChange (Prelude.Maybe [ResourceChangeDetail])
resourceChange_details = Lens.lens (\ResourceChange' {details} -> details) (\s@ResourceChange' {} a -> s {details = a} :: ResourceChange) Prelude.. Lens.mapping Lens.coerced

-- | If the change type is @Modify@, indicates whether the existing resource
-- is deleted and replaced with a new one.
resourceChange_replacement :: Lens.Lens' ResourceChange (Prelude.Maybe Replacement)
resourceChange_replacement = Lens.lens (\ResourceChange' {replacement} -> replacement) (\s@ResourceChange' {} a -> s {replacement = a} :: ResourceChange)

instance Core.FromJSON ResourceChange where
  parseJSON =
    Core.withObject
      "ResourceChange"
      ( \x ->
          ResourceChange'
            Prelude.<$> (x Core..:? "LogicalResourceId")
            Prelude.<*> (x Core..:? "PhysicalResourceId")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "Action")
            Prelude.<*> (x Core..:? "Scope" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Details" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Replacement")
      )

instance Prelude.Hashable ResourceChange where
  hashWithSalt salt' ResourceChange' {..} =
    salt' `Prelude.hashWithSalt` replacement
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` logicalResourceId

instance Prelude.NFData ResourceChange where
  rnf ResourceChange' {..} =
    Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf replacement
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf physicalResourceId
