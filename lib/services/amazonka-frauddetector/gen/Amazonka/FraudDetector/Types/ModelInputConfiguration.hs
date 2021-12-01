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
-- Module      : Amazonka.FraudDetector.Types.ModelInputConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelInputConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.ModelInputDataFormat
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon SageMaker model input configuration.
--
-- /See:/ 'newModelInputConfiguration' smart constructor.
data ModelInputConfiguration = ModelInputConfiguration'
  { -- | The format of the model input configuration. The format differs
    -- depending on if it is passed through to SageMaker or constructed by
    -- Amazon Fraud Detector.
    format :: Prelude.Maybe ModelInputDataFormat,
    -- | The event type name.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | Template for constructing the CSV input-data sent to SageMaker. At
    -- event-evaluation, the placeholders for variable-names in the template
    -- will be replaced with the variable values before being sent to
    -- SageMaker.
    csvInputTemplate :: Prelude.Maybe Prelude.Text,
    -- | Template for constructing the JSON input-data sent to SageMaker. At
    -- event-evaluation, the placeholders for variable names in the template
    -- will be replaced with the variable values before being sent to
    -- SageMaker.
    jsonInputTemplate :: Prelude.Maybe Prelude.Text,
    -- | The event variables.
    useEventVariables :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelInputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'modelInputConfiguration_format' - The format of the model input configuration. The format differs
-- depending on if it is passed through to SageMaker or constructed by
-- Amazon Fraud Detector.
--
-- 'eventTypeName', 'modelInputConfiguration_eventTypeName' - The event type name.
--
-- 'csvInputTemplate', 'modelInputConfiguration_csvInputTemplate' - Template for constructing the CSV input-data sent to SageMaker. At
-- event-evaluation, the placeholders for variable-names in the template
-- will be replaced with the variable values before being sent to
-- SageMaker.
--
-- 'jsonInputTemplate', 'modelInputConfiguration_jsonInputTemplate' - Template for constructing the JSON input-data sent to SageMaker. At
-- event-evaluation, the placeholders for variable names in the template
-- will be replaced with the variable values before being sent to
-- SageMaker.
--
-- 'useEventVariables', 'modelInputConfiguration_useEventVariables' - The event variables.
newModelInputConfiguration ::
  -- | 'useEventVariables'
  Prelude.Bool ->
  ModelInputConfiguration
newModelInputConfiguration pUseEventVariables_ =
  ModelInputConfiguration'
    { format = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      csvInputTemplate = Prelude.Nothing,
      jsonInputTemplate = Prelude.Nothing,
      useEventVariables = pUseEventVariables_
    }

-- | The format of the model input configuration. The format differs
-- depending on if it is passed through to SageMaker or constructed by
-- Amazon Fraud Detector.
modelInputConfiguration_format :: Lens.Lens' ModelInputConfiguration (Prelude.Maybe ModelInputDataFormat)
modelInputConfiguration_format = Lens.lens (\ModelInputConfiguration' {format} -> format) (\s@ModelInputConfiguration' {} a -> s {format = a} :: ModelInputConfiguration)

-- | The event type name.
modelInputConfiguration_eventTypeName :: Lens.Lens' ModelInputConfiguration (Prelude.Maybe Prelude.Text)
modelInputConfiguration_eventTypeName = Lens.lens (\ModelInputConfiguration' {eventTypeName} -> eventTypeName) (\s@ModelInputConfiguration' {} a -> s {eventTypeName = a} :: ModelInputConfiguration)

-- | Template for constructing the CSV input-data sent to SageMaker. At
-- event-evaluation, the placeholders for variable-names in the template
-- will be replaced with the variable values before being sent to
-- SageMaker.
modelInputConfiguration_csvInputTemplate :: Lens.Lens' ModelInputConfiguration (Prelude.Maybe Prelude.Text)
modelInputConfiguration_csvInputTemplate = Lens.lens (\ModelInputConfiguration' {csvInputTemplate} -> csvInputTemplate) (\s@ModelInputConfiguration' {} a -> s {csvInputTemplate = a} :: ModelInputConfiguration)

-- | Template for constructing the JSON input-data sent to SageMaker. At
-- event-evaluation, the placeholders for variable names in the template
-- will be replaced with the variable values before being sent to
-- SageMaker.
modelInputConfiguration_jsonInputTemplate :: Lens.Lens' ModelInputConfiguration (Prelude.Maybe Prelude.Text)
modelInputConfiguration_jsonInputTemplate = Lens.lens (\ModelInputConfiguration' {jsonInputTemplate} -> jsonInputTemplate) (\s@ModelInputConfiguration' {} a -> s {jsonInputTemplate = a} :: ModelInputConfiguration)

-- | The event variables.
modelInputConfiguration_useEventVariables :: Lens.Lens' ModelInputConfiguration Prelude.Bool
modelInputConfiguration_useEventVariables = Lens.lens (\ModelInputConfiguration' {useEventVariables} -> useEventVariables) (\s@ModelInputConfiguration' {} a -> s {useEventVariables = a} :: ModelInputConfiguration)

instance Core.FromJSON ModelInputConfiguration where
  parseJSON =
    Core.withObject
      "ModelInputConfiguration"
      ( \x ->
          ModelInputConfiguration'
            Prelude.<$> (x Core..:? "format")
            Prelude.<*> (x Core..:? "eventTypeName")
            Prelude.<*> (x Core..:? "csvInputTemplate")
            Prelude.<*> (x Core..:? "jsonInputTemplate")
            Prelude.<*> (x Core..: "useEventVariables")
      )

instance Prelude.Hashable ModelInputConfiguration where
  hashWithSalt salt' ModelInputConfiguration' {..} =
    salt' `Prelude.hashWithSalt` useEventVariables
      `Prelude.hashWithSalt` jsonInputTemplate
      `Prelude.hashWithSalt` csvInputTemplate
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` format

instance Prelude.NFData ModelInputConfiguration where
  rnf ModelInputConfiguration' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf useEventVariables
      `Prelude.seq` Prelude.rnf jsonInputTemplate
      `Prelude.seq` Prelude.rnf csvInputTemplate
      `Prelude.seq` Prelude.rnf eventTypeName

instance Core.ToJSON ModelInputConfiguration where
  toJSON ModelInputConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("format" Core..=) Prelude.<$> format,
            ("eventTypeName" Core..=) Prelude.<$> eventTypeName,
            ("csvInputTemplate" Core..=)
              Prelude.<$> csvInputTemplate,
            ("jsonInputTemplate" Core..=)
              Prelude.<$> jsonInputTemplate,
            Prelude.Just
              ("useEventVariables" Core..= useEventVariables)
          ]
      )
