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
-- Module      : Amazonka.Firehose.Types.DataFormatConversionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.DataFormatConversionConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.Firehose.Types.InputFormatConfiguration
import Amazonka.Firehose.Types.OutputFormatConfiguration
import Amazonka.Firehose.Types.SchemaConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies that you want Kinesis Data Firehose to convert data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
-- Kinesis Data Firehose uses the serializer and deserializer that you
-- specify, in addition to the column information from the AWS Glue table,
-- to deserialize your input data from JSON and then serialize it to the
-- Parquet or ORC format. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/record-format-conversion.html Kinesis Data Firehose Record Format Conversion>.
--
-- /See:/ 'newDataFormatConversionConfiguration' smart constructor.
data DataFormatConversionConfiguration = DataFormatConversionConfiguration'
  { -- | Specifies the serializer that you want Kinesis Data Firehose to use to
    -- convert the format of your data to the Parquet or ORC format. This
    -- parameter is required if @Enabled@ is set to true.
    outputFormatConfiguration :: Prelude.Maybe OutputFormatConfiguration,
    -- | Defaults to @true@. Set it to @false@ if you want to disable format
    -- conversion while preserving the configuration details.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the AWS Glue Data Catalog table that contains the column
    -- information. This parameter is required if @Enabled@ is set to true.
    schemaConfiguration :: Prelude.Maybe SchemaConfiguration,
    -- | Specifies the deserializer that you want Kinesis Data Firehose to use to
    -- convert the format of your data from JSON. This parameter is required if
    -- @Enabled@ is set to true.
    inputFormatConfiguration :: Prelude.Maybe InputFormatConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataFormatConversionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputFormatConfiguration', 'dataFormatConversionConfiguration_outputFormatConfiguration' - Specifies the serializer that you want Kinesis Data Firehose to use to
-- convert the format of your data to the Parquet or ORC format. This
-- parameter is required if @Enabled@ is set to true.
--
-- 'enabled', 'dataFormatConversionConfiguration_enabled' - Defaults to @true@. Set it to @false@ if you want to disable format
-- conversion while preserving the configuration details.
--
-- 'schemaConfiguration', 'dataFormatConversionConfiguration_schemaConfiguration' - Specifies the AWS Glue Data Catalog table that contains the column
-- information. This parameter is required if @Enabled@ is set to true.
--
-- 'inputFormatConfiguration', 'dataFormatConversionConfiguration_inputFormatConfiguration' - Specifies the deserializer that you want Kinesis Data Firehose to use to
-- convert the format of your data from JSON. This parameter is required if
-- @Enabled@ is set to true.
newDataFormatConversionConfiguration ::
  DataFormatConversionConfiguration
newDataFormatConversionConfiguration =
  DataFormatConversionConfiguration'
    { outputFormatConfiguration =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      schemaConfiguration = Prelude.Nothing,
      inputFormatConfiguration =
        Prelude.Nothing
    }

-- | Specifies the serializer that you want Kinesis Data Firehose to use to
-- convert the format of your data to the Parquet or ORC format. This
-- parameter is required if @Enabled@ is set to true.
dataFormatConversionConfiguration_outputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe OutputFormatConfiguration)
dataFormatConversionConfiguration_outputFormatConfiguration = Lens.lens (\DataFormatConversionConfiguration' {outputFormatConfiguration} -> outputFormatConfiguration) (\s@DataFormatConversionConfiguration' {} a -> s {outputFormatConfiguration = a} :: DataFormatConversionConfiguration)

-- | Defaults to @true@. Set it to @false@ if you want to disable format
-- conversion while preserving the configuration details.
dataFormatConversionConfiguration_enabled :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe Prelude.Bool)
dataFormatConversionConfiguration_enabled = Lens.lens (\DataFormatConversionConfiguration' {enabled} -> enabled) (\s@DataFormatConversionConfiguration' {} a -> s {enabled = a} :: DataFormatConversionConfiguration)

-- | Specifies the AWS Glue Data Catalog table that contains the column
-- information. This parameter is required if @Enabled@ is set to true.
dataFormatConversionConfiguration_schemaConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe SchemaConfiguration)
dataFormatConversionConfiguration_schemaConfiguration = Lens.lens (\DataFormatConversionConfiguration' {schemaConfiguration} -> schemaConfiguration) (\s@DataFormatConversionConfiguration' {} a -> s {schemaConfiguration = a} :: DataFormatConversionConfiguration)

-- | Specifies the deserializer that you want Kinesis Data Firehose to use to
-- convert the format of your data from JSON. This parameter is required if
-- @Enabled@ is set to true.
dataFormatConversionConfiguration_inputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe InputFormatConfiguration)
dataFormatConversionConfiguration_inputFormatConfiguration = Lens.lens (\DataFormatConversionConfiguration' {inputFormatConfiguration} -> inputFormatConfiguration) (\s@DataFormatConversionConfiguration' {} a -> s {inputFormatConfiguration = a} :: DataFormatConversionConfiguration)

instance
  Core.FromJSON
    DataFormatConversionConfiguration
  where
  parseJSON =
    Core.withObject
      "DataFormatConversionConfiguration"
      ( \x ->
          DataFormatConversionConfiguration'
            Prelude.<$> (x Core..:? "OutputFormatConfiguration")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "SchemaConfiguration")
            Prelude.<*> (x Core..:? "InputFormatConfiguration")
      )

instance
  Prelude.Hashable
    DataFormatConversionConfiguration
  where
  hashWithSalt
    salt'
    DataFormatConversionConfiguration' {..} =
      salt'
        `Prelude.hashWithSalt` inputFormatConfiguration
        `Prelude.hashWithSalt` schemaConfiguration
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` outputFormatConfiguration

instance
  Prelude.NFData
    DataFormatConversionConfiguration
  where
  rnf DataFormatConversionConfiguration' {..} =
    Prelude.rnf outputFormatConfiguration
      `Prelude.seq` Prelude.rnf inputFormatConfiguration
      `Prelude.seq` Prelude.rnf schemaConfiguration
      `Prelude.seq` Prelude.rnf enabled

instance
  Core.ToJSON
    DataFormatConversionConfiguration
  where
  toJSON DataFormatConversionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OutputFormatConfiguration" Core..=)
              Prelude.<$> outputFormatConfiguration,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("SchemaConfiguration" Core..=)
              Prelude.<$> schemaConfiguration,
            ("InputFormatConfiguration" Core..=)
              Prelude.<$> inputFormatConfiguration
          ]
      )
