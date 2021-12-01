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
-- Module      : Amazonka.AppRunner.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.EncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom encryption key that App Runner uses to encrypt copies
-- of the source repository and service logs.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The ARN of the KMS key that\'s used for encryption.
    kmsKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'encryptionConfiguration_kmsKey' - The ARN of the KMS key that\'s used for encryption.
newEncryptionConfiguration ::
  -- | 'kmsKey'
  Prelude.Text ->
  EncryptionConfiguration
newEncryptionConfiguration pKmsKey_ =
  EncryptionConfiguration' {kmsKey = pKmsKey_}

-- | The ARN of the KMS key that\'s used for encryption.
encryptionConfiguration_kmsKey :: Lens.Lens' EncryptionConfiguration Prelude.Text
encryptionConfiguration_kmsKey = Lens.lens (\EncryptionConfiguration' {kmsKey} -> kmsKey) (\s@EncryptionConfiguration' {} a -> s {kmsKey = a} :: EncryptionConfiguration)

instance Core.FromJSON EncryptionConfiguration where
  parseJSON =
    Core.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Core..: "KmsKey")
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt salt' EncryptionConfiguration' {..} =
    salt' `Prelude.hashWithSalt` kmsKey

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} = Prelude.rnf kmsKey

instance Core.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("KmsKey" Core..= kmsKey)]
      )
