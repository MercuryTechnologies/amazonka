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
-- Module      : Amazonka.SageMaker.Types.LambdaStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LambdaStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.OutputParameter

-- | Metadata for a Lambda step.
--
-- /See:/ 'newLambdaStepMetadata' smart constructor.
data LambdaStepMetadata = LambdaStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the Lambda function that was run by
    -- this step execution.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of the output parameters of the Lambda step.
    outputParameters :: Prelude.Maybe [OutputParameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'lambdaStepMetadata_arn' - The Amazon Resource Name (ARN) of the Lambda function that was run by
-- this step execution.
--
-- 'outputParameters', 'lambdaStepMetadata_outputParameters' - A list of the output parameters of the Lambda step.
newLambdaStepMetadata ::
  LambdaStepMetadata
newLambdaStepMetadata =
  LambdaStepMetadata'
    { arn = Prelude.Nothing,
      outputParameters = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Lambda function that was run by
-- this step execution.
lambdaStepMetadata_arn :: Lens.Lens' LambdaStepMetadata (Prelude.Maybe Prelude.Text)
lambdaStepMetadata_arn = Lens.lens (\LambdaStepMetadata' {arn} -> arn) (\s@LambdaStepMetadata' {} a -> s {arn = a} :: LambdaStepMetadata)

-- | A list of the output parameters of the Lambda step.
lambdaStepMetadata_outputParameters :: Lens.Lens' LambdaStepMetadata (Prelude.Maybe [OutputParameter])
lambdaStepMetadata_outputParameters = Lens.lens (\LambdaStepMetadata' {outputParameters} -> outputParameters) (\s@LambdaStepMetadata' {} a -> s {outputParameters = a} :: LambdaStepMetadata) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LambdaStepMetadata where
  parseJSON =
    Core.withObject
      "LambdaStepMetadata"
      ( \x ->
          LambdaStepMetadata'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> ( x Core..:? "OutputParameters"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LambdaStepMetadata where
  hashWithSalt salt' LambdaStepMetadata' {..} =
    salt' `Prelude.hashWithSalt` outputParameters
      `Prelude.hashWithSalt` arn

instance Prelude.NFData LambdaStepMetadata where
  rnf LambdaStepMetadata' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf outputParameters
