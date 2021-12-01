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
-- Module      : Amazonka.SWF.Types.WorkflowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a workflow type.
--
-- /See:/ 'newWorkflowType' smart constructor.
data WorkflowType = WorkflowType'
  { -- | The name of the workflow type.
    --
    -- The combination of workflow type name and version must be unique with in
    -- a domain.
    name :: Prelude.Text,
    -- | The version of the workflow type.
    --
    -- The combination of workflow type name and version must be unique with in
    -- a domain.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'workflowType_name' - The name of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
--
-- 'version', 'workflowType_version' - The version of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
newWorkflowType ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  WorkflowType
newWorkflowType pName_ pVersion_ =
  WorkflowType' {name = pName_, version = pVersion_}

-- | The name of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
workflowType_name :: Lens.Lens' WorkflowType Prelude.Text
workflowType_name = Lens.lens (\WorkflowType' {name} -> name) (\s@WorkflowType' {} a -> s {name = a} :: WorkflowType)

-- | The version of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
workflowType_version :: Lens.Lens' WorkflowType Prelude.Text
workflowType_version = Lens.lens (\WorkflowType' {version} -> version) (\s@WorkflowType' {} a -> s {version = a} :: WorkflowType)

instance Core.FromJSON WorkflowType where
  parseJSON =
    Core.withObject
      "WorkflowType"
      ( \x ->
          WorkflowType'
            Prelude.<$> (x Core..: "name") Prelude.<*> (x Core..: "version")
      )

instance Prelude.Hashable WorkflowType where
  hashWithSalt salt' WorkflowType' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData WorkflowType where
  rnf WorkflowType' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Core.ToJSON WorkflowType where
  toJSON WorkflowType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("version" Core..= version)
          ]
      )
