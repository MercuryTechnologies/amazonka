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
-- Module      : Amazonka.KinesisAnalytics.Types.OutputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.OutputUpdate where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalytics.Types.DestinationSchema
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Amazonka.KinesisAnalytics.Types.LambdaOutputUpdate
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to the output configuration identified by the
-- @OutputId@.
--
-- /See:/ 'newOutputUpdate' smart constructor.
data OutputUpdate = OutputUpdate'
  { -- | Describes an Amazon Kinesis stream as the destination for the output.
    kinesisStreamsOutputUpdate :: Prelude.Maybe KinesisStreamsOutputUpdate,
    -- | Describes the data format when records are written to the destination.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
    destinationSchemaUpdate :: Prelude.Maybe DestinationSchema,
    -- | Describes an Amazon Kinesis Firehose delivery stream as the destination
    -- for the output.
    kinesisFirehoseOutputUpdate :: Prelude.Maybe KinesisFirehoseOutputUpdate,
    -- | If you want to specify a different in-application stream for this output
    -- configuration, use this field to specify the new in-application stream
    -- name.
    nameUpdate :: Prelude.Maybe Prelude.Text,
    -- | Describes an AWS Lambda function as the destination for the output.
    lambdaOutputUpdate :: Prelude.Maybe LambdaOutputUpdate,
    -- | Identifies the specific output configuration that you want to update.
    outputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisStreamsOutputUpdate', 'outputUpdate_kinesisStreamsOutputUpdate' - Describes an Amazon Kinesis stream as the destination for the output.
--
-- 'destinationSchemaUpdate', 'outputUpdate_destinationSchemaUpdate' - Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
--
-- 'kinesisFirehoseOutputUpdate', 'outputUpdate_kinesisFirehoseOutputUpdate' - Describes an Amazon Kinesis Firehose delivery stream as the destination
-- for the output.
--
-- 'nameUpdate', 'outputUpdate_nameUpdate' - If you want to specify a different in-application stream for this output
-- configuration, use this field to specify the new in-application stream
-- name.
--
-- 'lambdaOutputUpdate', 'outputUpdate_lambdaOutputUpdate' - Describes an AWS Lambda function as the destination for the output.
--
-- 'outputId', 'outputUpdate_outputId' - Identifies the specific output configuration that you want to update.
newOutputUpdate ::
  -- | 'outputId'
  Prelude.Text ->
  OutputUpdate
newOutputUpdate pOutputId_ =
  OutputUpdate'
    { kinesisStreamsOutputUpdate =
        Prelude.Nothing,
      destinationSchemaUpdate = Prelude.Nothing,
      kinesisFirehoseOutputUpdate = Prelude.Nothing,
      nameUpdate = Prelude.Nothing,
      lambdaOutputUpdate = Prelude.Nothing,
      outputId = pOutputId_
    }

-- | Describes an Amazon Kinesis stream as the destination for the output.
outputUpdate_kinesisStreamsOutputUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe KinesisStreamsOutputUpdate)
outputUpdate_kinesisStreamsOutputUpdate = Lens.lens (\OutputUpdate' {kinesisStreamsOutputUpdate} -> kinesisStreamsOutputUpdate) (\s@OutputUpdate' {} a -> s {kinesisStreamsOutputUpdate = a} :: OutputUpdate)

-- | Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
outputUpdate_destinationSchemaUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe DestinationSchema)
outputUpdate_destinationSchemaUpdate = Lens.lens (\OutputUpdate' {destinationSchemaUpdate} -> destinationSchemaUpdate) (\s@OutputUpdate' {} a -> s {destinationSchemaUpdate = a} :: OutputUpdate)

-- | Describes an Amazon Kinesis Firehose delivery stream as the destination
-- for the output.
outputUpdate_kinesisFirehoseOutputUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe KinesisFirehoseOutputUpdate)
outputUpdate_kinesisFirehoseOutputUpdate = Lens.lens (\OutputUpdate' {kinesisFirehoseOutputUpdate} -> kinesisFirehoseOutputUpdate) (\s@OutputUpdate' {} a -> s {kinesisFirehoseOutputUpdate = a} :: OutputUpdate)

-- | If you want to specify a different in-application stream for this output
-- configuration, use this field to specify the new in-application stream
-- name.
outputUpdate_nameUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe Prelude.Text)
outputUpdate_nameUpdate = Lens.lens (\OutputUpdate' {nameUpdate} -> nameUpdate) (\s@OutputUpdate' {} a -> s {nameUpdate = a} :: OutputUpdate)

-- | Describes an AWS Lambda function as the destination for the output.
outputUpdate_lambdaOutputUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe LambdaOutputUpdate)
outputUpdate_lambdaOutputUpdate = Lens.lens (\OutputUpdate' {lambdaOutputUpdate} -> lambdaOutputUpdate) (\s@OutputUpdate' {} a -> s {lambdaOutputUpdate = a} :: OutputUpdate)

-- | Identifies the specific output configuration that you want to update.
outputUpdate_outputId :: Lens.Lens' OutputUpdate Prelude.Text
outputUpdate_outputId = Lens.lens (\OutputUpdate' {outputId} -> outputId) (\s@OutputUpdate' {} a -> s {outputId = a} :: OutputUpdate)

instance Prelude.Hashable OutputUpdate where
  hashWithSalt salt' OutputUpdate' {..} =
    salt' `Prelude.hashWithSalt` outputId
      `Prelude.hashWithSalt` lambdaOutputUpdate
      `Prelude.hashWithSalt` nameUpdate
      `Prelude.hashWithSalt` kinesisFirehoseOutputUpdate
      `Prelude.hashWithSalt` destinationSchemaUpdate
      `Prelude.hashWithSalt` kinesisStreamsOutputUpdate

instance Prelude.NFData OutputUpdate where
  rnf OutputUpdate' {..} =
    Prelude.rnf kinesisStreamsOutputUpdate
      `Prelude.seq` Prelude.rnf outputId
      `Prelude.seq` Prelude.rnf lambdaOutputUpdate
      `Prelude.seq` Prelude.rnf nameUpdate
      `Prelude.seq` Prelude.rnf kinesisFirehoseOutputUpdate
      `Prelude.seq` Prelude.rnf destinationSchemaUpdate

instance Core.ToJSON OutputUpdate where
  toJSON OutputUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KinesisStreamsOutputUpdate" Core..=)
              Prelude.<$> kinesisStreamsOutputUpdate,
            ("DestinationSchemaUpdate" Core..=)
              Prelude.<$> destinationSchemaUpdate,
            ("KinesisFirehoseOutputUpdate" Core..=)
              Prelude.<$> kinesisFirehoseOutputUpdate,
            ("NameUpdate" Core..=) Prelude.<$> nameUpdate,
            ("LambdaOutputUpdate" Core..=)
              Prelude.<$> lambdaOutputUpdate,
            Prelude.Just ("OutputId" Core..= outputId)
          ]
      )
