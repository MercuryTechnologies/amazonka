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
-- Module      : Amazonka.EMR.Types.Step
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Step where

import qualified Amazonka.Core as Core
import Amazonka.EMR.Types.ActionOnFailure
import Amazonka.EMR.Types.HadoopStepConfig
import Amazonka.EMR.Types.StepStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This represents a step in a cluster.
--
-- /See:/ 'newStep' smart constructor.
data Step = Step'
  { -- | The current execution status details of the cluster step.
    status :: Prelude.Maybe StepStatus,
    -- | The action to take when the cluster step fails. Possible values are
    -- @TERMINATE_CLUSTER@, @CANCEL_AND_WAIT@, and @CONTINUE@.
    -- @TERMINATE_JOB_FLOW@ is provided for backward compatibility. We
    -- recommend using @TERMINATE_CLUSTER@ instead.
    --
    -- If a cluster\'s @StepConcurrencyLevel@ is greater than @1@, do not use
    -- @AddJobFlowSteps@ to submit a step with this parameter set to
    -- @CANCEL_AND_WAIT@ or @TERMINATE_CLUSTER@. The step is not submitted and
    -- the action fails with a message that the @ActionOnFailure@ setting is
    -- not valid.
    --
    -- If you change a cluster\'s @StepConcurrencyLevel@ to be greater than 1
    -- while a step is running, the @ActionOnFailure@ parameter may not behave
    -- as you expect. In this case, for a step that fails with this parameter
    -- set to @CANCEL_AND_WAIT@, pending steps and the running step are not
    -- canceled; for a step that fails with this parameter set to
    -- @TERMINATE_CLUSTER@, the cluster does not terminate.
    actionOnFailure :: Prelude.Maybe ActionOnFailure,
    -- | The Hadoop job configuration of the cluster step.
    config :: Prelude.Maybe HadoopStepConfig,
    -- | The name of the cluster step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the cluster step.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Step' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'step_status' - The current execution status details of the cluster step.
--
-- 'actionOnFailure', 'step_actionOnFailure' - The action to take when the cluster step fails. Possible values are
-- @TERMINATE_CLUSTER@, @CANCEL_AND_WAIT@, and @CONTINUE@.
-- @TERMINATE_JOB_FLOW@ is provided for backward compatibility. We
-- recommend using @TERMINATE_CLUSTER@ instead.
--
-- If a cluster\'s @StepConcurrencyLevel@ is greater than @1@, do not use
-- @AddJobFlowSteps@ to submit a step with this parameter set to
-- @CANCEL_AND_WAIT@ or @TERMINATE_CLUSTER@. The step is not submitted and
-- the action fails with a message that the @ActionOnFailure@ setting is
-- not valid.
--
-- If you change a cluster\'s @StepConcurrencyLevel@ to be greater than 1
-- while a step is running, the @ActionOnFailure@ parameter may not behave
-- as you expect. In this case, for a step that fails with this parameter
-- set to @CANCEL_AND_WAIT@, pending steps and the running step are not
-- canceled; for a step that fails with this parameter set to
-- @TERMINATE_CLUSTER@, the cluster does not terminate.
--
-- 'config', 'step_config' - The Hadoop job configuration of the cluster step.
--
-- 'name', 'step_name' - The name of the cluster step.
--
-- 'id', 'step_id' - The identifier of the cluster step.
newStep ::
  Step
newStep =
  Step'
    { status = Prelude.Nothing,
      actionOnFailure = Prelude.Nothing,
      config = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The current execution status details of the cluster step.
step_status :: Lens.Lens' Step (Prelude.Maybe StepStatus)
step_status = Lens.lens (\Step' {status} -> status) (\s@Step' {} a -> s {status = a} :: Step)

-- | The action to take when the cluster step fails. Possible values are
-- @TERMINATE_CLUSTER@, @CANCEL_AND_WAIT@, and @CONTINUE@.
-- @TERMINATE_JOB_FLOW@ is provided for backward compatibility. We
-- recommend using @TERMINATE_CLUSTER@ instead.
--
-- If a cluster\'s @StepConcurrencyLevel@ is greater than @1@, do not use
-- @AddJobFlowSteps@ to submit a step with this parameter set to
-- @CANCEL_AND_WAIT@ or @TERMINATE_CLUSTER@. The step is not submitted and
-- the action fails with a message that the @ActionOnFailure@ setting is
-- not valid.
--
-- If you change a cluster\'s @StepConcurrencyLevel@ to be greater than 1
-- while a step is running, the @ActionOnFailure@ parameter may not behave
-- as you expect. In this case, for a step that fails with this parameter
-- set to @CANCEL_AND_WAIT@, pending steps and the running step are not
-- canceled; for a step that fails with this parameter set to
-- @TERMINATE_CLUSTER@, the cluster does not terminate.
step_actionOnFailure :: Lens.Lens' Step (Prelude.Maybe ActionOnFailure)
step_actionOnFailure = Lens.lens (\Step' {actionOnFailure} -> actionOnFailure) (\s@Step' {} a -> s {actionOnFailure = a} :: Step)

-- | The Hadoop job configuration of the cluster step.
step_config :: Lens.Lens' Step (Prelude.Maybe HadoopStepConfig)
step_config = Lens.lens (\Step' {config} -> config) (\s@Step' {} a -> s {config = a} :: Step)

-- | The name of the cluster step.
step_name :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_name = Lens.lens (\Step' {name} -> name) (\s@Step' {} a -> s {name = a} :: Step)

-- | The identifier of the cluster step.
step_id :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_id = Lens.lens (\Step' {id} -> id) (\s@Step' {} a -> s {id = a} :: Step)

instance Core.FromJSON Step where
  parseJSON =
    Core.withObject
      "Step"
      ( \x ->
          Step'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ActionOnFailure")
            Prelude.<*> (x Core..:? "Config")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable Step where
  hashWithSalt salt' Step' {..} =
    salt' `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` config
      `Prelude.hashWithSalt` actionOnFailure
      `Prelude.hashWithSalt` status

instance Prelude.NFData Step where
  rnf Step' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf config
      `Prelude.seq` Prelude.rnf actionOnFailure
