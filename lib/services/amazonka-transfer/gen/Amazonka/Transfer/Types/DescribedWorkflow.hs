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
-- Module      : Amazonka.Transfer.Types.DescribedWorkflow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedWorkflow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.Tag
import Amazonka.Transfer.Types.WorkflowStep

-- | Describes the properties of the specified workflow
--
-- /See:/ 'newDescribedWorkflow' smart constructor.
data DescribedWorkflow = DescribedWorkflow'
  { -- | Specifies the steps (actions) to take if errors are encountered during
    -- execution of the workflow.
    onExceptionSteps :: Prelude.Maybe [WorkflowStep],
    -- | Specifies the details for the steps that are in the specified workflow.
    steps :: Prelude.Maybe [WorkflowStep],
    -- | A unique identifier for the workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the text description for the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs that can be used to group and search for workflows. Tags
    -- are metadata attached to workflows for any purpose.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the unique Amazon Resource Name (ARN) for the workflow.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onExceptionSteps', 'describedWorkflow_onExceptionSteps' - Specifies the steps (actions) to take if errors are encountered during
-- execution of the workflow.
--
-- 'steps', 'describedWorkflow_steps' - Specifies the details for the steps that are in the specified workflow.
--
-- 'workflowId', 'describedWorkflow_workflowId' - A unique identifier for the workflow.
--
-- 'description', 'describedWorkflow_description' - Specifies the text description for the workflow.
--
-- 'tags', 'describedWorkflow_tags' - Key-value pairs that can be used to group and search for workflows. Tags
-- are metadata attached to workflows for any purpose.
--
-- 'arn', 'describedWorkflow_arn' - Specifies the unique Amazon Resource Name (ARN) for the workflow.
newDescribedWorkflow ::
  -- | 'arn'
  Prelude.Text ->
  DescribedWorkflow
newDescribedWorkflow pArn_ =
  DescribedWorkflow'
    { onExceptionSteps =
        Prelude.Nothing,
      steps = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      arn = pArn_
    }

-- | Specifies the steps (actions) to take if errors are encountered during
-- execution of the workflow.
describedWorkflow_onExceptionSteps :: Lens.Lens' DescribedWorkflow (Prelude.Maybe [WorkflowStep])
describedWorkflow_onExceptionSteps = Lens.lens (\DescribedWorkflow' {onExceptionSteps} -> onExceptionSteps) (\s@DescribedWorkflow' {} a -> s {onExceptionSteps = a} :: DescribedWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the details for the steps that are in the specified workflow.
describedWorkflow_steps :: Lens.Lens' DescribedWorkflow (Prelude.Maybe [WorkflowStep])
describedWorkflow_steps = Lens.lens (\DescribedWorkflow' {steps} -> steps) (\s@DescribedWorkflow' {} a -> s {steps = a} :: DescribedWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the workflow.
describedWorkflow_workflowId :: Lens.Lens' DescribedWorkflow (Prelude.Maybe Prelude.Text)
describedWorkflow_workflowId = Lens.lens (\DescribedWorkflow' {workflowId} -> workflowId) (\s@DescribedWorkflow' {} a -> s {workflowId = a} :: DescribedWorkflow)

-- | Specifies the text description for the workflow.
describedWorkflow_description :: Lens.Lens' DescribedWorkflow (Prelude.Maybe Prelude.Text)
describedWorkflow_description = Lens.lens (\DescribedWorkflow' {description} -> description) (\s@DescribedWorkflow' {} a -> s {description = a} :: DescribedWorkflow)

-- | Key-value pairs that can be used to group and search for workflows. Tags
-- are metadata attached to workflows for any purpose.
describedWorkflow_tags :: Lens.Lens' DescribedWorkflow (Prelude.Maybe (Prelude.NonEmpty Tag))
describedWorkflow_tags = Lens.lens (\DescribedWorkflow' {tags} -> tags) (\s@DescribedWorkflow' {} a -> s {tags = a} :: DescribedWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the unique Amazon Resource Name (ARN) for the workflow.
describedWorkflow_arn :: Lens.Lens' DescribedWorkflow Prelude.Text
describedWorkflow_arn = Lens.lens (\DescribedWorkflow' {arn} -> arn) (\s@DescribedWorkflow' {} a -> s {arn = a} :: DescribedWorkflow)

instance Core.FromJSON DescribedWorkflow where
  parseJSON =
    Core.withObject
      "DescribedWorkflow"
      ( \x ->
          DescribedWorkflow'
            Prelude.<$> ( x Core..:? "OnExceptionSteps"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Steps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "WorkflowId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags")
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable DescribedWorkflow where
  hashWithSalt salt' DescribedWorkflow' {..} =
    salt' `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` steps
      `Prelude.hashWithSalt` onExceptionSteps

instance Prelude.NFData DescribedWorkflow where
  rnf DescribedWorkflow' {..} =
    Prelude.rnf onExceptionSteps
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf steps
