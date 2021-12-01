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
-- Module      : Amazonka.Macie.Types.FailedS3Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.FailedS3Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Macie.Types.S3Resource
import qualified Amazonka.Prelude as Prelude

-- | Includes details about the failed S3 resources.
--
-- /See:/ 'newFailedS3Resource' smart constructor.
data FailedS3Resource = FailedS3Resource'
  { -- | The status code of a failed item.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message of a failed item.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The failed S3 resources.
    failedItem :: Prelude.Maybe S3Resource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedS3Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedS3Resource_errorCode' - The status code of a failed item.
--
-- 'errorMessage', 'failedS3Resource_errorMessage' - The error message of a failed item.
--
-- 'failedItem', 'failedS3Resource_failedItem' - The failed S3 resources.
newFailedS3Resource ::
  FailedS3Resource
newFailedS3Resource =
  FailedS3Resource'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      failedItem = Prelude.Nothing
    }

-- | The status code of a failed item.
failedS3Resource_errorCode :: Lens.Lens' FailedS3Resource (Prelude.Maybe Prelude.Text)
failedS3Resource_errorCode = Lens.lens (\FailedS3Resource' {errorCode} -> errorCode) (\s@FailedS3Resource' {} a -> s {errorCode = a} :: FailedS3Resource)

-- | The error message of a failed item.
failedS3Resource_errorMessage :: Lens.Lens' FailedS3Resource (Prelude.Maybe Prelude.Text)
failedS3Resource_errorMessage = Lens.lens (\FailedS3Resource' {errorMessage} -> errorMessage) (\s@FailedS3Resource' {} a -> s {errorMessage = a} :: FailedS3Resource)

-- | The failed S3 resources.
failedS3Resource_failedItem :: Lens.Lens' FailedS3Resource (Prelude.Maybe S3Resource)
failedS3Resource_failedItem = Lens.lens (\FailedS3Resource' {failedItem} -> failedItem) (\s@FailedS3Resource' {} a -> s {failedItem = a} :: FailedS3Resource)

instance Core.FromJSON FailedS3Resource where
  parseJSON =
    Core.withObject
      "FailedS3Resource"
      ( \x ->
          FailedS3Resource'
            Prelude.<$> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "failedItem")
      )

instance Prelude.Hashable FailedS3Resource where
  hashWithSalt salt' FailedS3Resource' {..} =
    salt' `Prelude.hashWithSalt` failedItem
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData FailedS3Resource where
  rnf FailedS3Resource' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf failedItem
      `Prelude.seq` Prelude.rnf errorMessage
