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
-- Module      : Amazonka.Kafka.Types.UnprocessedScramSecret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.UnprocessedScramSecret where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Error info for scram secret associate\/disassociate failure.
--
-- /See:/ 'newUnprocessedScramSecret' smart constructor.
data UnprocessedScramSecret = UnprocessedScramSecret'
  { -- | Error code for associate\/disassociate failure.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Error message for associate\/disassociate failure.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | AWS Secrets Manager secret ARN.
    secretArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedScramSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'unprocessedScramSecret_errorCode' - Error code for associate\/disassociate failure.
--
-- 'errorMessage', 'unprocessedScramSecret_errorMessage' - Error message for associate\/disassociate failure.
--
-- 'secretArn', 'unprocessedScramSecret_secretArn' - AWS Secrets Manager secret ARN.
newUnprocessedScramSecret ::
  UnprocessedScramSecret
newUnprocessedScramSecret =
  UnprocessedScramSecret'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      secretArn = Prelude.Nothing
    }

-- | Error code for associate\/disassociate failure.
unprocessedScramSecret_errorCode :: Lens.Lens' UnprocessedScramSecret (Prelude.Maybe Prelude.Text)
unprocessedScramSecret_errorCode = Lens.lens (\UnprocessedScramSecret' {errorCode} -> errorCode) (\s@UnprocessedScramSecret' {} a -> s {errorCode = a} :: UnprocessedScramSecret)

-- | Error message for associate\/disassociate failure.
unprocessedScramSecret_errorMessage :: Lens.Lens' UnprocessedScramSecret (Prelude.Maybe Prelude.Text)
unprocessedScramSecret_errorMessage = Lens.lens (\UnprocessedScramSecret' {errorMessage} -> errorMessage) (\s@UnprocessedScramSecret' {} a -> s {errorMessage = a} :: UnprocessedScramSecret)

-- | AWS Secrets Manager secret ARN.
unprocessedScramSecret_secretArn :: Lens.Lens' UnprocessedScramSecret (Prelude.Maybe Prelude.Text)
unprocessedScramSecret_secretArn = Lens.lens (\UnprocessedScramSecret' {secretArn} -> secretArn) (\s@UnprocessedScramSecret' {} a -> s {secretArn = a} :: UnprocessedScramSecret)

instance Core.FromJSON UnprocessedScramSecret where
  parseJSON =
    Core.withObject
      "UnprocessedScramSecret"
      ( \x ->
          UnprocessedScramSecret'
            Prelude.<$> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "secretArn")
      )

instance Prelude.Hashable UnprocessedScramSecret where
  hashWithSalt salt' UnprocessedScramSecret' {..} =
    salt' `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData UnprocessedScramSecret where
  rnf UnprocessedScramSecret' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf errorMessage
