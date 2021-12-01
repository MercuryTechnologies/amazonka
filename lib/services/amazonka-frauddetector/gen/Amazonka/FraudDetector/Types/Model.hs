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
-- Module      : Amazonka.FraudDetector.Types.Model
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.Model where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.ModelTypeEnum
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The model.
--
-- /See:/ 'newModel' smart constructor.
data Model = Model'
  { -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | Timestamp of last time the model was updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the model.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the model was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | The model description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Model' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelType', 'model_modelType' - The model type.
--
-- 'lastUpdatedTime', 'model_lastUpdatedTime' - Timestamp of last time the model was updated.
--
-- 'modelId', 'model_modelId' - The model ID.
--
-- 'arn', 'model_arn' - The ARN of the model.
--
-- 'createdTime', 'model_createdTime' - Timestamp of when the model was created.
--
-- 'eventTypeName', 'model_eventTypeName' - The name of the event type.
--
-- 'description', 'model_description' - The model description.
newModel ::
  Model
newModel =
  Model'
    { modelType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      modelId = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The model type.
model_modelType :: Lens.Lens' Model (Prelude.Maybe ModelTypeEnum)
model_modelType = Lens.lens (\Model' {modelType} -> modelType) (\s@Model' {} a -> s {modelType = a} :: Model)

-- | Timestamp of last time the model was updated.
model_lastUpdatedTime :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_lastUpdatedTime = Lens.lens (\Model' {lastUpdatedTime} -> lastUpdatedTime) (\s@Model' {} a -> s {lastUpdatedTime = a} :: Model)

-- | The model ID.
model_modelId :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_modelId = Lens.lens (\Model' {modelId} -> modelId) (\s@Model' {} a -> s {modelId = a} :: Model)

-- | The ARN of the model.
model_arn :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_arn = Lens.lens (\Model' {arn} -> arn) (\s@Model' {} a -> s {arn = a} :: Model)

-- | Timestamp of when the model was created.
model_createdTime :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_createdTime = Lens.lens (\Model' {createdTime} -> createdTime) (\s@Model' {} a -> s {createdTime = a} :: Model)

-- | The name of the event type.
model_eventTypeName :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_eventTypeName = Lens.lens (\Model' {eventTypeName} -> eventTypeName) (\s@Model' {} a -> s {eventTypeName = a} :: Model)

-- | The model description.
model_description :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_description = Lens.lens (\Model' {description} -> description) (\s@Model' {} a -> s {description = a} :: Model)

instance Core.FromJSON Model where
  parseJSON =
    Core.withObject
      "Model"
      ( \x ->
          Model'
            Prelude.<$> (x Core..:? "modelType")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "modelId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "eventTypeName")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable Model where
  hashWithSalt salt' Model' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` modelType

instance Prelude.NFData Model where
  rnf Model' {..} =
    Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
