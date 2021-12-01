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
-- Module      : Amazonka.ComprehendMedical.Types.RxNormEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.RxNormEntity where

import Amazonka.ComprehendMedical.Types.RxNormAttribute
import Amazonka.ComprehendMedical.Types.RxNormConcept
import Amazonka.ComprehendMedical.Types.RxNormEntityCategory
import Amazonka.ComprehendMedical.Types.RxNormEntityType
import Amazonka.ComprehendMedical.Types.RxNormTrait
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The collection of medical entities extracted from the input text and
-- their associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence that Amazon Comprehend Medical has in the
-- detection and analysis. Attributes and traits of the entity are also
-- returned.
--
-- /See:/ 'newRxNormEntity' smart constructor.
data RxNormEntity = RxNormEntity'
  { -- | The RxNorm concepts that the entity could refer to, along with a score
    -- indicating the likelihood of the match.
    rxNormConcepts :: Prelude.Maybe [RxNormConcept],
    -- | The 0-based character offset in the input text that shows where the
    -- entity begins. The offset returns the UTF-8 code point in the string.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The segment of input text extracted from which the entity was detected.
    text :: Prelude.Maybe Prelude.Text,
    -- | The category of the entity. The recognized categories are @GENERIC@ or
    -- @BRAND_NAME@.
    category :: Prelude.Maybe RxNormEntityCategory,
    -- | The level of confidence that Amazon Comprehend Medical has in the
    -- accuracy of the detected entity.
    score :: Prelude.Maybe Prelude.Double,
    -- | Contextual information for the entity.
    traits :: Prelude.Maybe [RxNormTrait],
    -- | The extracted attributes that relate to the entity. The attributes
    -- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
    -- @RATE@, @ROUTE_OR_MODE@, and @STRENGTH@.
    attributes :: Prelude.Maybe [RxNormAttribute],
    -- | The 0-based character offset in the input text that shows where the
    -- entity ends. The offset returns the UTF-8 code point in the string.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The numeric identifier for the entity. This is a monotonically
    -- increasing id unique within this response rather than a global unique
    -- identifier.
    id :: Prelude.Maybe Prelude.Int,
    -- | Describes the specific type of entity. For InferRxNorm, the recognized
    -- entity type is @MEDICATION@.
    type' :: Prelude.Maybe RxNormEntityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RxNormEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rxNormConcepts', 'rxNormEntity_rxNormConcepts' - The RxNorm concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
--
-- 'beginOffset', 'rxNormEntity_beginOffset' - The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
--
-- 'text', 'rxNormEntity_text' - The segment of input text extracted from which the entity was detected.
--
-- 'category', 'rxNormEntity_category' - The category of the entity. The recognized categories are @GENERIC@ or
-- @BRAND_NAME@.
--
-- 'score', 'rxNormEntity_score' - The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected entity.
--
-- 'traits', 'rxNormEntity_traits' - Contextual information for the entity.
--
-- 'attributes', 'rxNormEntity_attributes' - The extracted attributes that relate to the entity. The attributes
-- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
-- @RATE@, @ROUTE_OR_MODE@, and @STRENGTH@.
--
-- 'endOffset', 'rxNormEntity_endOffset' - The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
--
-- 'id', 'rxNormEntity_id' - The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
--
-- 'type'', 'rxNormEntity_type' - Describes the specific type of entity. For InferRxNorm, the recognized
-- entity type is @MEDICATION@.
newRxNormEntity ::
  RxNormEntity
newRxNormEntity =
  RxNormEntity'
    { rxNormConcepts = Prelude.Nothing,
      beginOffset = Prelude.Nothing,
      text = Prelude.Nothing,
      category = Prelude.Nothing,
      score = Prelude.Nothing,
      traits = Prelude.Nothing,
      attributes = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The RxNorm concepts that the entity could refer to, along with a score
-- indicating the likelihood of the match.
rxNormEntity_rxNormConcepts :: Lens.Lens' RxNormEntity (Prelude.Maybe [RxNormConcept])
rxNormEntity_rxNormConcepts = Lens.lens (\RxNormEntity' {rxNormConcepts} -> rxNormConcepts) (\s@RxNormEntity' {} a -> s {rxNormConcepts = a} :: RxNormEntity) Prelude.. Lens.mapping Lens.coerced

-- | The 0-based character offset in the input text that shows where the
-- entity begins. The offset returns the UTF-8 code point in the string.
rxNormEntity_beginOffset :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Int)
rxNormEntity_beginOffset = Lens.lens (\RxNormEntity' {beginOffset} -> beginOffset) (\s@RxNormEntity' {} a -> s {beginOffset = a} :: RxNormEntity)

-- | The segment of input text extracted from which the entity was detected.
rxNormEntity_text :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Text)
rxNormEntity_text = Lens.lens (\RxNormEntity' {text} -> text) (\s@RxNormEntity' {} a -> s {text = a} :: RxNormEntity)

-- | The category of the entity. The recognized categories are @GENERIC@ or
-- @BRAND_NAME@.
rxNormEntity_category :: Lens.Lens' RxNormEntity (Prelude.Maybe RxNormEntityCategory)
rxNormEntity_category = Lens.lens (\RxNormEntity' {category} -> category) (\s@RxNormEntity' {} a -> s {category = a} :: RxNormEntity)

-- | The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected entity.
rxNormEntity_score :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Double)
rxNormEntity_score = Lens.lens (\RxNormEntity' {score} -> score) (\s@RxNormEntity' {} a -> s {score = a} :: RxNormEntity)

-- | Contextual information for the entity.
rxNormEntity_traits :: Lens.Lens' RxNormEntity (Prelude.Maybe [RxNormTrait])
rxNormEntity_traits = Lens.lens (\RxNormEntity' {traits} -> traits) (\s@RxNormEntity' {} a -> s {traits = a} :: RxNormEntity) Prelude.. Lens.mapping Lens.coerced

-- | The extracted attributes that relate to the entity. The attributes
-- recognized by InferRxNorm are @DOSAGE@, @DURATION@, @FORM@, @FREQUENCY@,
-- @RATE@, @ROUTE_OR_MODE@, and @STRENGTH@.
rxNormEntity_attributes :: Lens.Lens' RxNormEntity (Prelude.Maybe [RxNormAttribute])
rxNormEntity_attributes = Lens.lens (\RxNormEntity' {attributes} -> attributes) (\s@RxNormEntity' {} a -> s {attributes = a} :: RxNormEntity) Prelude.. Lens.mapping Lens.coerced

-- | The 0-based character offset in the input text that shows where the
-- entity ends. The offset returns the UTF-8 code point in the string.
rxNormEntity_endOffset :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Int)
rxNormEntity_endOffset = Lens.lens (\RxNormEntity' {endOffset} -> endOffset) (\s@RxNormEntity' {} a -> s {endOffset = a} :: RxNormEntity)

-- | The numeric identifier for the entity. This is a monotonically
-- increasing id unique within this response rather than a global unique
-- identifier.
rxNormEntity_id :: Lens.Lens' RxNormEntity (Prelude.Maybe Prelude.Int)
rxNormEntity_id = Lens.lens (\RxNormEntity' {id} -> id) (\s@RxNormEntity' {} a -> s {id = a} :: RxNormEntity)

-- | Describes the specific type of entity. For InferRxNorm, the recognized
-- entity type is @MEDICATION@.
rxNormEntity_type :: Lens.Lens' RxNormEntity (Prelude.Maybe RxNormEntityType)
rxNormEntity_type = Lens.lens (\RxNormEntity' {type'} -> type') (\s@RxNormEntity' {} a -> s {type' = a} :: RxNormEntity)

instance Core.FromJSON RxNormEntity where
  parseJSON =
    Core.withObject
      "RxNormEntity"
      ( \x ->
          RxNormEntity'
            Prelude.<$> (x Core..:? "RxNormConcepts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Category")
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "Traits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable RxNormEntity where
  hashWithSalt salt' RxNormEntity' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` traits
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` rxNormConcepts

instance Prelude.NFData RxNormEntity where
  rnf RxNormEntity' {..} =
    Prelude.rnf rxNormConcepts
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf traits
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf beginOffset
