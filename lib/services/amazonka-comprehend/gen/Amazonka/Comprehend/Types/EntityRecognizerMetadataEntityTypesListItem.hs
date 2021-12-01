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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem where

import Amazonka.Comprehend.Types.EntityTypesEvaluationMetrics
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Individual item from the list of entity types in the metadata of an
-- entity recognizer.
--
-- /See:/ 'newEntityRecognizerMetadataEntityTypesListItem' smart constructor.
data EntityRecognizerMetadataEntityTypesListItem = EntityRecognizerMetadataEntityTypesListItem'
  { -- | Detailed information about the accuracy of the entity recognizer for a
    -- specific item on the list of entity types.
    evaluationMetrics :: Prelude.Maybe EntityTypesEvaluationMetrics,
    -- | Type of entity from the list of entity types in the metadata of an
    -- entity recognizer.
    type' :: Prelude.Maybe Prelude.Text,
    -- | Indicates the number of times the given entity type was seen in the
    -- training data.
    numberOfTrainMentions :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerMetadataEntityTypesListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationMetrics', 'entityRecognizerMetadataEntityTypesListItem_evaluationMetrics' - Detailed information about the accuracy of the entity recognizer for a
-- specific item on the list of entity types.
--
-- 'type'', 'entityRecognizerMetadataEntityTypesListItem_type' - Type of entity from the list of entity types in the metadata of an
-- entity recognizer.
--
-- 'numberOfTrainMentions', 'entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions' - Indicates the number of times the given entity type was seen in the
-- training data.
newEntityRecognizerMetadataEntityTypesListItem ::
  EntityRecognizerMetadataEntityTypesListItem
newEntityRecognizerMetadataEntityTypesListItem =
  EntityRecognizerMetadataEntityTypesListItem'
    { evaluationMetrics =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      numberOfTrainMentions =
        Prelude.Nothing
    }

-- | Detailed information about the accuracy of the entity recognizer for a
-- specific item on the list of entity types.
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe EntityTypesEvaluationMetrics)
entityRecognizerMetadataEntityTypesListItem_evaluationMetrics = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {evaluationMetrics} -> evaluationMetrics) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {evaluationMetrics = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Type of entity from the list of entity types in the metadata of an
-- entity recognizer.
entityRecognizerMetadataEntityTypesListItem_type :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe Prelude.Text)
entityRecognizerMetadataEntityTypesListItem_type = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {type'} -> type') (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {type' = a} :: EntityRecognizerMetadataEntityTypesListItem)

-- | Indicates the number of times the given entity type was seen in the
-- training data.
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Prelude.Maybe Prelude.Int)
entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions = Lens.lens (\EntityRecognizerMetadataEntityTypesListItem' {numberOfTrainMentions} -> numberOfTrainMentions) (\s@EntityRecognizerMetadataEntityTypesListItem' {} a -> s {numberOfTrainMentions = a} :: EntityRecognizerMetadataEntityTypesListItem)

instance
  Core.FromJSON
    EntityRecognizerMetadataEntityTypesListItem
  where
  parseJSON =
    Core.withObject
      "EntityRecognizerMetadataEntityTypesListItem"
      ( \x ->
          EntityRecognizerMetadataEntityTypesListItem'
            Prelude.<$> (x Core..:? "EvaluationMetrics")
              Prelude.<*> (x Core..:? "Type")
              Prelude.<*> (x Core..:? "NumberOfTrainMentions")
      )

instance
  Prelude.Hashable
    EntityRecognizerMetadataEntityTypesListItem
  where
  hashWithSalt
    salt'
    EntityRecognizerMetadataEntityTypesListItem' {..} =
      salt' `Prelude.hashWithSalt` numberOfTrainMentions
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` evaluationMetrics

instance
  Prelude.NFData
    EntityRecognizerMetadataEntityTypesListItem
  where
  rnf EntityRecognizerMetadataEntityTypesListItem' {..} =
    Prelude.rnf evaluationMetrics
      `Prelude.seq` Prelude.rnf numberOfTrainMentions
      `Prelude.seq` Prelude.rnf type'
