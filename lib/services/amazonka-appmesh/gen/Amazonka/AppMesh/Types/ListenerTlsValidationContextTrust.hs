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
-- Module      : Amazonka.AppMesh.Types.ListenerTlsValidationContextTrust
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTlsValidationContextTrust where

import Amazonka.AppMesh.Types.TlsValidationContextFileTrust
import Amazonka.AppMesh.Types.TlsValidationContextSdsTrust
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener\'s Transport Layer Security (TLS)
-- validation context trust.
--
-- /See:/ 'newListenerTlsValidationContextTrust' smart constructor.
data ListenerTlsValidationContextTrust = ListenerTlsValidationContextTrust'
  { -- | A reference to an object that represents a listener\'s Transport Layer
    -- Security (TLS) Secret Discovery Service validation context trust.
    sds :: Prelude.Maybe TlsValidationContextSdsTrust,
    -- | An object that represents a Transport Layer Security (TLS) validation
    -- context trust for a local file.
    file :: Prelude.Maybe TlsValidationContextFileTrust
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTlsValidationContextTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sds', 'listenerTlsValidationContextTrust_sds' - A reference to an object that represents a listener\'s Transport Layer
-- Security (TLS) Secret Discovery Service validation context trust.
--
-- 'file', 'listenerTlsValidationContextTrust_file' - An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
newListenerTlsValidationContextTrust ::
  ListenerTlsValidationContextTrust
newListenerTlsValidationContextTrust =
  ListenerTlsValidationContextTrust'
    { sds =
        Prelude.Nothing,
      file = Prelude.Nothing
    }

-- | A reference to an object that represents a listener\'s Transport Layer
-- Security (TLS) Secret Discovery Service validation context trust.
listenerTlsValidationContextTrust_sds :: Lens.Lens' ListenerTlsValidationContextTrust (Prelude.Maybe TlsValidationContextSdsTrust)
listenerTlsValidationContextTrust_sds = Lens.lens (\ListenerTlsValidationContextTrust' {sds} -> sds) (\s@ListenerTlsValidationContextTrust' {} a -> s {sds = a} :: ListenerTlsValidationContextTrust)

-- | An object that represents a Transport Layer Security (TLS) validation
-- context trust for a local file.
listenerTlsValidationContextTrust_file :: Lens.Lens' ListenerTlsValidationContextTrust (Prelude.Maybe TlsValidationContextFileTrust)
listenerTlsValidationContextTrust_file = Lens.lens (\ListenerTlsValidationContextTrust' {file} -> file) (\s@ListenerTlsValidationContextTrust' {} a -> s {file = a} :: ListenerTlsValidationContextTrust)

instance
  Core.FromJSON
    ListenerTlsValidationContextTrust
  where
  parseJSON =
    Core.withObject
      "ListenerTlsValidationContextTrust"
      ( \x ->
          ListenerTlsValidationContextTrust'
            Prelude.<$> (x Core..:? "sds") Prelude.<*> (x Core..:? "file")
      )

instance
  Prelude.Hashable
    ListenerTlsValidationContextTrust
  where
  hashWithSalt
    salt'
    ListenerTlsValidationContextTrust' {..} =
      salt' `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` sds

instance
  Prelude.NFData
    ListenerTlsValidationContextTrust
  where
  rnf ListenerTlsValidationContextTrust' {..} =
    Prelude.rnf sds `Prelude.seq` Prelude.rnf file

instance
  Core.ToJSON
    ListenerTlsValidationContextTrust
  where
  toJSON ListenerTlsValidationContextTrust' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sds" Core..=) Prelude.<$> sds,
            ("file" Core..=) Prelude.<$> file
          ]
      )
