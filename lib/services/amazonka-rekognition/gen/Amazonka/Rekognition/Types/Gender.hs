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
-- Module      : Amazonka.Rekognition.Types.Gender
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Gender where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.GenderType

-- | The predicted gender of a detected face.
--
-- Amazon Rekognition makes gender binary (male\/female) predictions based
-- on the physical appearance of a face in a particular image. This kind of
-- prediction is not designed to categorize a person’s gender identity, and
-- you shouldn\'t use Amazon Rekognition to make such a determination. For
-- example, a male actor wearing a long-haired wig and earrings for a role
-- might be predicted as female.
--
-- Using Amazon Rekognition to make gender binary predictions is best
-- suited for use cases where aggregate gender distribution statistics need
-- to be analyzed without identifying specific users. For example, the
-- percentage of female users compared to male users on a social media
-- platform.
--
-- We don\'t recommend using gender binary predictions to make decisions
-- that impact  an individual\'s rights, privacy, or access to services.
--
-- /See:/ 'newGender' smart constructor.
data Gender = Gender'
  { -- | The predicted gender of the face.
    value :: Prelude.Maybe GenderType,
    -- | Level of confidence in the prediction.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Gender' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'gender_value' - The predicted gender of the face.
--
-- 'confidence', 'gender_confidence' - Level of confidence in the prediction.
newGender ::
  Gender
newGender =
  Gender'
    { value = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | The predicted gender of the face.
gender_value :: Lens.Lens' Gender (Prelude.Maybe GenderType)
gender_value = Lens.lens (\Gender' {value} -> value) (\s@Gender' {} a -> s {value = a} :: Gender)

-- | Level of confidence in the prediction.
gender_confidence :: Lens.Lens' Gender (Prelude.Maybe Prelude.Double)
gender_confidence = Lens.lens (\Gender' {confidence} -> confidence) (\s@Gender' {} a -> s {confidence = a} :: Gender)

instance Core.FromJSON Gender where
  parseJSON =
    Core.withObject
      "Gender"
      ( \x ->
          Gender'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable Gender where
  hashWithSalt salt' Gender' {..} =
    salt' `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` value

instance Prelude.NFData Gender where
  rnf Gender' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf confidence
