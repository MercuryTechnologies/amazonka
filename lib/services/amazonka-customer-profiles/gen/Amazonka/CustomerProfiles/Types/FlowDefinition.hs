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
-- Module      : Amazonka.CustomerProfiles.Types.FlowDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.FlowDefinition where

import qualified Amazonka.Core as Core
import Amazonka.CustomerProfiles.Types.SourceFlowConfig
import Amazonka.CustomerProfiles.Types.Task
import Amazonka.CustomerProfiles.Types.TriggerConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configurations that control how Customer Profiles retrieves data
-- from the source, Amazon AppFlow. Customer Profiles uses this information
-- to create an AppFlow flow on behalf of customers.
--
-- /See:/ 'newFlowDefinition' smart constructor.
data FlowDefinition = FlowDefinition'
  { -- | A description of the flow you want to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified name of the flow. Use underscores (_) or hyphens (-) only.
    -- Spaces are not allowed.
    flowName :: Prelude.Text,
    -- | The Amazon Resource Name of the AWS Key Management Service (KMS) key you
    -- provide for encryption.
    kmsArn :: Prelude.Text,
    -- | The configuration that controls how Customer Profiles retrieves data
    -- from the source.
    sourceFlowConfig :: SourceFlowConfig,
    -- | A list of tasks that Customer Profiles performs while transferring the
    -- data in the flow run.
    tasks :: [Task],
    -- | The trigger settings that determine how and when the flow runs.
    triggerConfig :: TriggerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'flowDefinition_description' - A description of the flow you want to create.
--
-- 'flowName', 'flowDefinition_flowName' - The specified name of the flow. Use underscores (_) or hyphens (-) only.
-- Spaces are not allowed.
--
-- 'kmsArn', 'flowDefinition_kmsArn' - The Amazon Resource Name of the AWS Key Management Service (KMS) key you
-- provide for encryption.
--
-- 'sourceFlowConfig', 'flowDefinition_sourceFlowConfig' - The configuration that controls how Customer Profiles retrieves data
-- from the source.
--
-- 'tasks', 'flowDefinition_tasks' - A list of tasks that Customer Profiles performs while transferring the
-- data in the flow run.
--
-- 'triggerConfig', 'flowDefinition_triggerConfig' - The trigger settings that determine how and when the flow runs.
newFlowDefinition ::
  -- | 'flowName'
  Prelude.Text ->
  -- | 'kmsArn'
  Prelude.Text ->
  -- | 'sourceFlowConfig'
  SourceFlowConfig ->
  -- | 'triggerConfig'
  TriggerConfig ->
  FlowDefinition
newFlowDefinition
  pFlowName_
  pKmsArn_
  pSourceFlowConfig_
  pTriggerConfig_ =
    FlowDefinition'
      { description = Prelude.Nothing,
        flowName = pFlowName_,
        kmsArn = pKmsArn_,
        sourceFlowConfig = pSourceFlowConfig_,
        tasks = Prelude.mempty,
        triggerConfig = pTriggerConfig_
      }

-- | A description of the flow you want to create.
flowDefinition_description :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_description = Lens.lens (\FlowDefinition' {description} -> description) (\s@FlowDefinition' {} a -> s {description = a} :: FlowDefinition)

-- | The specified name of the flow. Use underscores (_) or hyphens (-) only.
-- Spaces are not allowed.
flowDefinition_flowName :: Lens.Lens' FlowDefinition Prelude.Text
flowDefinition_flowName = Lens.lens (\FlowDefinition' {flowName} -> flowName) (\s@FlowDefinition' {} a -> s {flowName = a} :: FlowDefinition)

-- | The Amazon Resource Name of the AWS Key Management Service (KMS) key you
-- provide for encryption.
flowDefinition_kmsArn :: Lens.Lens' FlowDefinition Prelude.Text
flowDefinition_kmsArn = Lens.lens (\FlowDefinition' {kmsArn} -> kmsArn) (\s@FlowDefinition' {} a -> s {kmsArn = a} :: FlowDefinition)

-- | The configuration that controls how Customer Profiles retrieves data
-- from the source.
flowDefinition_sourceFlowConfig :: Lens.Lens' FlowDefinition SourceFlowConfig
flowDefinition_sourceFlowConfig = Lens.lens (\FlowDefinition' {sourceFlowConfig} -> sourceFlowConfig) (\s@FlowDefinition' {} a -> s {sourceFlowConfig = a} :: FlowDefinition)

-- | A list of tasks that Customer Profiles performs while transferring the
-- data in the flow run.
flowDefinition_tasks :: Lens.Lens' FlowDefinition [Task]
flowDefinition_tasks = Lens.lens (\FlowDefinition' {tasks} -> tasks) (\s@FlowDefinition' {} a -> s {tasks = a} :: FlowDefinition) Prelude.. Lens.coerced

-- | The trigger settings that determine how and when the flow runs.
flowDefinition_triggerConfig :: Lens.Lens' FlowDefinition TriggerConfig
flowDefinition_triggerConfig = Lens.lens (\FlowDefinition' {triggerConfig} -> triggerConfig) (\s@FlowDefinition' {} a -> s {triggerConfig = a} :: FlowDefinition)

instance Prelude.Hashable FlowDefinition where
  hashWithSalt salt' FlowDefinition' {..} =
    salt' `Prelude.hashWithSalt` triggerConfig
      `Prelude.hashWithSalt` tasks
      `Prelude.hashWithSalt` sourceFlowConfig
      `Prelude.hashWithSalt` kmsArn
      `Prelude.hashWithSalt` flowName
      `Prelude.hashWithSalt` description

instance Prelude.NFData FlowDefinition where
  rnf FlowDefinition' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf triggerConfig
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf sourceFlowConfig
      `Prelude.seq` Prelude.rnf kmsArn
      `Prelude.seq` Prelude.rnf flowName

instance Core.ToJSON FlowDefinition where
  toJSON FlowDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("FlowName" Core..= flowName),
            Prelude.Just ("KmsArn" Core..= kmsArn),
            Prelude.Just
              ("SourceFlowConfig" Core..= sourceFlowConfig),
            Prelude.Just ("Tasks" Core..= tasks),
            Prelude.Just
              ("TriggerConfig" Core..= triggerConfig)
          ]
      )
