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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.Input
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.Input where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.InputParallelism
import Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfiguration
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInput
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInput
import Amazonka.KinesisAnalyticsV2.Types.SourceSchema
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | When you configure the application input for a SQL-based Kinesis Data
-- Analytics application, you specify the streaming source, the
-- in-application stream name that is created, and the mapping between the
-- two.
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | Describes the number of in-application streams to create.
    inputParallelism :: Prelude.Maybe InputParallelism,
    -- | The InputProcessingConfiguration for the input. An input processor
    -- transforms records as they are received from the stream, before the
    -- application\'s SQL code executes. Currently, the only input processing
    -- configuration available is InputLambdaProcessor.
    inputProcessingConfiguration :: Prelude.Maybe InputProcessingConfiguration,
    -- | If the streaming source is an Amazon Kinesis data stream, identifies the
    -- stream\'s Amazon Resource Name (ARN).
    kinesisStreamsInput :: Prelude.Maybe KinesisStreamsInput,
    -- | If the streaming source is an Amazon Kinesis Data Firehose delivery
    -- stream, identifies the delivery stream\'s ARN.
    kinesisFirehoseInput :: Prelude.Maybe KinesisFirehoseInput,
    -- | The name prefix to use when creating an in-application stream. Suppose
    -- that you specify a prefix \"@MyInApplicationStream@.\" Kinesis Data
    -- Analytics then creates one or more (as per the @InputParallelism@ count
    -- you specified) in-application streams with the names
    -- \"@MyInApplicationStream_001@,\" \"@MyInApplicationStream_002@,\" and so
    -- on.
    namePrefix :: Prelude.Text,
    -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns in the in-application stream
    -- that is being created.
    --
    -- Also used to describe the format of the reference data source.
    inputSchema :: SourceSchema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputParallelism', 'input_inputParallelism' - Describes the number of in-application streams to create.
--
-- 'inputProcessingConfiguration', 'input_inputProcessingConfiguration' - The InputProcessingConfiguration for the input. An input processor
-- transforms records as they are received from the stream, before the
-- application\'s SQL code executes. Currently, the only input processing
-- configuration available is InputLambdaProcessor.
--
-- 'kinesisStreamsInput', 'input_kinesisStreamsInput' - If the streaming source is an Amazon Kinesis data stream, identifies the
-- stream\'s Amazon Resource Name (ARN).
--
-- 'kinesisFirehoseInput', 'input_kinesisFirehoseInput' - If the streaming source is an Amazon Kinesis Data Firehose delivery
-- stream, identifies the delivery stream\'s ARN.
--
-- 'namePrefix', 'input_namePrefix' - The name prefix to use when creating an in-application stream. Suppose
-- that you specify a prefix \"@MyInApplicationStream@.\" Kinesis Data
-- Analytics then creates one or more (as per the @InputParallelism@ count
-- you specified) in-application streams with the names
-- \"@MyInApplicationStream_001@,\" \"@MyInApplicationStream_002@,\" and so
-- on.
--
-- 'inputSchema', 'input_inputSchema' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
--
-- Also used to describe the format of the reference data source.
newInput ::
  -- | 'namePrefix'
  Prelude.Text ->
  -- | 'inputSchema'
  SourceSchema ->
  Input
newInput pNamePrefix_ pInputSchema_ =
  Input'
    { inputParallelism = Prelude.Nothing,
      inputProcessingConfiguration = Prelude.Nothing,
      kinesisStreamsInput = Prelude.Nothing,
      kinesisFirehoseInput = Prelude.Nothing,
      namePrefix = pNamePrefix_,
      inputSchema = pInputSchema_
    }

-- | Describes the number of in-application streams to create.
input_inputParallelism :: Lens.Lens' Input (Prelude.Maybe InputParallelism)
input_inputParallelism = Lens.lens (\Input' {inputParallelism} -> inputParallelism) (\s@Input' {} a -> s {inputParallelism = a} :: Input)

-- | The InputProcessingConfiguration for the input. An input processor
-- transforms records as they are received from the stream, before the
-- application\'s SQL code executes. Currently, the only input processing
-- configuration available is InputLambdaProcessor.
input_inputProcessingConfiguration :: Lens.Lens' Input (Prelude.Maybe InputProcessingConfiguration)
input_inputProcessingConfiguration = Lens.lens (\Input' {inputProcessingConfiguration} -> inputProcessingConfiguration) (\s@Input' {} a -> s {inputProcessingConfiguration = a} :: Input)

-- | If the streaming source is an Amazon Kinesis data stream, identifies the
-- stream\'s Amazon Resource Name (ARN).
input_kinesisStreamsInput :: Lens.Lens' Input (Prelude.Maybe KinesisStreamsInput)
input_kinesisStreamsInput = Lens.lens (\Input' {kinesisStreamsInput} -> kinesisStreamsInput) (\s@Input' {} a -> s {kinesisStreamsInput = a} :: Input)

-- | If the streaming source is an Amazon Kinesis Data Firehose delivery
-- stream, identifies the delivery stream\'s ARN.
input_kinesisFirehoseInput :: Lens.Lens' Input (Prelude.Maybe KinesisFirehoseInput)
input_kinesisFirehoseInput = Lens.lens (\Input' {kinesisFirehoseInput} -> kinesisFirehoseInput) (\s@Input' {} a -> s {kinesisFirehoseInput = a} :: Input)

-- | The name prefix to use when creating an in-application stream. Suppose
-- that you specify a prefix \"@MyInApplicationStream@.\" Kinesis Data
-- Analytics then creates one or more (as per the @InputParallelism@ count
-- you specified) in-application streams with the names
-- \"@MyInApplicationStream_001@,\" \"@MyInApplicationStream_002@,\" and so
-- on.
input_namePrefix :: Lens.Lens' Input Prelude.Text
input_namePrefix = Lens.lens (\Input' {namePrefix} -> namePrefix) (\s@Input' {} a -> s {namePrefix = a} :: Input)

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
--
-- Also used to describe the format of the reference data source.
input_inputSchema :: Lens.Lens' Input SourceSchema
input_inputSchema = Lens.lens (\Input' {inputSchema} -> inputSchema) (\s@Input' {} a -> s {inputSchema = a} :: Input)

instance Prelude.Hashable Input where
  hashWithSalt salt' Input' {..} =
    salt' `Prelude.hashWithSalt` inputSchema
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` kinesisFirehoseInput
      `Prelude.hashWithSalt` kinesisStreamsInput
      `Prelude.hashWithSalt` inputProcessingConfiguration
      `Prelude.hashWithSalt` inputParallelism

instance Prelude.NFData Input where
  rnf Input' {..} =
    Prelude.rnf inputParallelism
      `Prelude.seq` Prelude.rnf inputSchema
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf kinesisFirehoseInput
      `Prelude.seq` Prelude.rnf kinesisStreamsInput
      `Prelude.seq` Prelude.rnf inputProcessingConfiguration

instance Core.ToJSON Input where
  toJSON Input' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InputParallelism" Core..=)
              Prelude.<$> inputParallelism,
            ("InputProcessingConfiguration" Core..=)
              Prelude.<$> inputProcessingConfiguration,
            ("KinesisStreamsInput" Core..=)
              Prelude.<$> kinesisStreamsInput,
            ("KinesisFirehoseInput" Core..=)
              Prelude.<$> kinesisFirehoseInput,
            Prelude.Just ("NamePrefix" Core..= namePrefix),
            Prelude.Just ("InputSchema" Core..= inputSchema)
          ]
      )
