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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceInputConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceInputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutEquipment.Types.InferenceInputNameConfiguration
import Amazonka.LookoutEquipment.Types.InferenceS3InputConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration information for the input data for the
-- inference, including S3 location of input data..
--
-- /See:/ 'newInferenceInputConfiguration' smart constructor.
data InferenceInputConfiguration = InferenceInputConfiguration'
  { -- | Indicates the difference between your time zone and Greenwich Mean Time
    -- (GMT).
    inputTimeZoneOffset :: Prelude.Maybe Prelude.Text,
    -- | Specifies configuration information for the input data for the
    -- inference, including S3 location of input data..
    s3InputConfiguration :: Prelude.Maybe InferenceS3InputConfiguration,
    -- | Specifies configuration information for the input data for the
    -- inference, including timestamp format and delimiter.
    inferenceInputNameConfiguration :: Prelude.Maybe InferenceInputNameConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceInputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputTimeZoneOffset', 'inferenceInputConfiguration_inputTimeZoneOffset' - Indicates the difference between your time zone and Greenwich Mean Time
-- (GMT).
--
-- 's3InputConfiguration', 'inferenceInputConfiguration_s3InputConfiguration' - Specifies configuration information for the input data for the
-- inference, including S3 location of input data..
--
-- 'inferenceInputNameConfiguration', 'inferenceInputConfiguration_inferenceInputNameConfiguration' - Specifies configuration information for the input data for the
-- inference, including timestamp format and delimiter.
newInferenceInputConfiguration ::
  InferenceInputConfiguration
newInferenceInputConfiguration =
  InferenceInputConfiguration'
    { inputTimeZoneOffset =
        Prelude.Nothing,
      s3InputConfiguration = Prelude.Nothing,
      inferenceInputNameConfiguration =
        Prelude.Nothing
    }

-- | Indicates the difference between your time zone and Greenwich Mean Time
-- (GMT).
inferenceInputConfiguration_inputTimeZoneOffset :: Lens.Lens' InferenceInputConfiguration (Prelude.Maybe Prelude.Text)
inferenceInputConfiguration_inputTimeZoneOffset = Lens.lens (\InferenceInputConfiguration' {inputTimeZoneOffset} -> inputTimeZoneOffset) (\s@InferenceInputConfiguration' {} a -> s {inputTimeZoneOffset = a} :: InferenceInputConfiguration)

-- | Specifies configuration information for the input data for the
-- inference, including S3 location of input data..
inferenceInputConfiguration_s3InputConfiguration :: Lens.Lens' InferenceInputConfiguration (Prelude.Maybe InferenceS3InputConfiguration)
inferenceInputConfiguration_s3InputConfiguration = Lens.lens (\InferenceInputConfiguration' {s3InputConfiguration} -> s3InputConfiguration) (\s@InferenceInputConfiguration' {} a -> s {s3InputConfiguration = a} :: InferenceInputConfiguration)

-- | Specifies configuration information for the input data for the
-- inference, including timestamp format and delimiter.
inferenceInputConfiguration_inferenceInputNameConfiguration :: Lens.Lens' InferenceInputConfiguration (Prelude.Maybe InferenceInputNameConfiguration)
inferenceInputConfiguration_inferenceInputNameConfiguration = Lens.lens (\InferenceInputConfiguration' {inferenceInputNameConfiguration} -> inferenceInputNameConfiguration) (\s@InferenceInputConfiguration' {} a -> s {inferenceInputNameConfiguration = a} :: InferenceInputConfiguration)

instance Core.FromJSON InferenceInputConfiguration where
  parseJSON =
    Core.withObject
      "InferenceInputConfiguration"
      ( \x ->
          InferenceInputConfiguration'
            Prelude.<$> (x Core..:? "InputTimeZoneOffset")
            Prelude.<*> (x Core..:? "S3InputConfiguration")
            Prelude.<*> (x Core..:? "InferenceInputNameConfiguration")
      )

instance Prelude.Hashable InferenceInputConfiguration where
  hashWithSalt salt' InferenceInputConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` inferenceInputNameConfiguration
      `Prelude.hashWithSalt` s3InputConfiguration
      `Prelude.hashWithSalt` inputTimeZoneOffset

instance Prelude.NFData InferenceInputConfiguration where
  rnf InferenceInputConfiguration' {..} =
    Prelude.rnf inputTimeZoneOffset
      `Prelude.seq` Prelude.rnf inferenceInputNameConfiguration
      `Prelude.seq` Prelude.rnf s3InputConfiguration

instance Core.ToJSON InferenceInputConfiguration where
  toJSON InferenceInputConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InputTimeZoneOffset" Core..=)
              Prelude.<$> inputTimeZoneOffset,
            ("S3InputConfiguration" Core..=)
              Prelude.<$> s3InputConfiguration,
            ("InferenceInputNameConfiguration" Core..=)
              Prelude.<$> inferenceInputNameConfiguration
          ]
      )
