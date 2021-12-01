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
-- Module      : Amazonka.IoT.Types.KeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.KeyPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a key pair.
--
-- /See:/ 'newKeyPair' smart constructor.
data KeyPair = KeyPair'
  { -- | The private key.
    privateKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The public key.
    publicKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateKey', 'keyPair_privateKey' - The private key.
--
-- 'publicKey', 'keyPair_publicKey' - The public key.
newKeyPair ::
  KeyPair
newKeyPair =
  KeyPair'
    { privateKey = Prelude.Nothing,
      publicKey = Prelude.Nothing
    }

-- | The private key.
keyPair_privateKey :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_privateKey = Lens.lens (\KeyPair' {privateKey} -> privateKey) (\s@KeyPair' {} a -> s {privateKey = a} :: KeyPair) Prelude.. Lens.mapping Core._Sensitive

-- | The public key.
keyPair_publicKey :: Lens.Lens' KeyPair (Prelude.Maybe Prelude.Text)
keyPair_publicKey = Lens.lens (\KeyPair' {publicKey} -> publicKey) (\s@KeyPair' {} a -> s {publicKey = a} :: KeyPair)

instance Core.FromJSON KeyPair where
  parseJSON =
    Core.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Prelude.<$> (x Core..:? "PrivateKey")
            Prelude.<*> (x Core..:? "PublicKey")
      )

instance Prelude.Hashable KeyPair where
  hashWithSalt salt' KeyPair' {..} =
    salt' `Prelude.hashWithSalt` publicKey
      `Prelude.hashWithSalt` privateKey

instance Prelude.NFData KeyPair where
  rnf KeyPair' {..} =
    Prelude.rnf privateKey
      `Prelude.seq` Prelude.rnf publicKey
