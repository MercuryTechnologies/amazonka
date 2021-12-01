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
-- Module      : Amazonka.FIS.Types.UpdateExperimentTemplateActionInputItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.UpdateExperimentTemplateActionInputItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies an action for an experiment template.
--
-- /See:/ 'newUpdateExperimentTemplateActionInputItem' smart constructor.
data UpdateExperimentTemplateActionInputItem = UpdateExperimentTemplateActionInputItem'
  { -- | The name of the action that must be completed before the current action
    -- starts. Omit this parameter to run the action at the start of the
    -- experiment.
    startAfter :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the action, if applicable.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description for the action.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateExperimentTemplateActionInputItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startAfter', 'updateExperimentTemplateActionInputItem_startAfter' - The name of the action that must be completed before the current action
-- starts. Omit this parameter to run the action at the start of the
-- experiment.
--
-- 'actionId', 'updateExperimentTemplateActionInputItem_actionId' - The ID of the action.
--
-- 'parameters', 'updateExperimentTemplateActionInputItem_parameters' - The parameters for the action, if applicable.
--
-- 'targets', 'updateExperimentTemplateActionInputItem_targets' - The targets for the action.
--
-- 'description', 'updateExperimentTemplateActionInputItem_description' - A description for the action.
newUpdateExperimentTemplateActionInputItem ::
  UpdateExperimentTemplateActionInputItem
newUpdateExperimentTemplateActionInputItem =
  UpdateExperimentTemplateActionInputItem'
    { startAfter =
        Prelude.Nothing,
      actionId = Prelude.Nothing,
      parameters = Prelude.Nothing,
      targets = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The name of the action that must be completed before the current action
-- starts. Omit this parameter to run the action at the start of the
-- experiment.
updateExperimentTemplateActionInputItem_startAfter :: Lens.Lens' UpdateExperimentTemplateActionInputItem (Prelude.Maybe [Prelude.Text])
updateExperimentTemplateActionInputItem_startAfter = Lens.lens (\UpdateExperimentTemplateActionInputItem' {startAfter} -> startAfter) (\s@UpdateExperimentTemplateActionInputItem' {} a -> s {startAfter = a} :: UpdateExperimentTemplateActionInputItem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the action.
updateExperimentTemplateActionInputItem_actionId :: Lens.Lens' UpdateExperimentTemplateActionInputItem (Prelude.Maybe Prelude.Text)
updateExperimentTemplateActionInputItem_actionId = Lens.lens (\UpdateExperimentTemplateActionInputItem' {actionId} -> actionId) (\s@UpdateExperimentTemplateActionInputItem' {} a -> s {actionId = a} :: UpdateExperimentTemplateActionInputItem)

-- | The parameters for the action, if applicable.
updateExperimentTemplateActionInputItem_parameters :: Lens.Lens' UpdateExperimentTemplateActionInputItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateExperimentTemplateActionInputItem_parameters = Lens.lens (\UpdateExperimentTemplateActionInputItem' {parameters} -> parameters) (\s@UpdateExperimentTemplateActionInputItem' {} a -> s {parameters = a} :: UpdateExperimentTemplateActionInputItem) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the action.
updateExperimentTemplateActionInputItem_targets :: Lens.Lens' UpdateExperimentTemplateActionInputItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateExperimentTemplateActionInputItem_targets = Lens.lens (\UpdateExperimentTemplateActionInputItem' {targets} -> targets) (\s@UpdateExperimentTemplateActionInputItem' {} a -> s {targets = a} :: UpdateExperimentTemplateActionInputItem) Prelude.. Lens.mapping Lens.coerced

-- | A description for the action.
updateExperimentTemplateActionInputItem_description :: Lens.Lens' UpdateExperimentTemplateActionInputItem (Prelude.Maybe Prelude.Text)
updateExperimentTemplateActionInputItem_description = Lens.lens (\UpdateExperimentTemplateActionInputItem' {description} -> description) (\s@UpdateExperimentTemplateActionInputItem' {} a -> s {description = a} :: UpdateExperimentTemplateActionInputItem)

instance
  Prelude.Hashable
    UpdateExperimentTemplateActionInputItem
  where
  hashWithSalt
    salt'
    UpdateExperimentTemplateActionInputItem' {..} =
      salt' `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` targets
        `Prelude.hashWithSalt` parameters
        `Prelude.hashWithSalt` actionId
        `Prelude.hashWithSalt` startAfter

instance
  Prelude.NFData
    UpdateExperimentTemplateActionInputItem
  where
  rnf UpdateExperimentTemplateActionInputItem' {..} =
    Prelude.rnf startAfter
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf actionId

instance
  Core.ToJSON
    UpdateExperimentTemplateActionInputItem
  where
  toJSON UpdateExperimentTemplateActionInputItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("startAfter" Core..=) Prelude.<$> startAfter,
            ("actionId" Core..=) Prelude.<$> actionId,
            ("parameters" Core..=) Prelude.<$> parameters,
            ("targets" Core..=) Prelude.<$> targets,
            ("description" Core..=) Prelude.<$> description
          ]
      )
