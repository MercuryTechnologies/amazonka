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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListenerTlsCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListenerTlsCertificate where

import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsAcmCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsFileCertificate
import Amazonka.AppMesh.Types.VirtualGatewayListenerTlsSdsCertificate
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener\'s Transport Layer Security (TLS)
-- certificate.
--
-- /See:/ 'newVirtualGatewayListenerTlsCertificate' smart constructor.
data VirtualGatewayListenerTlsCertificate = VirtualGatewayListenerTlsCertificate'
  { -- | A reference to an object that represents an Certificate Manager
    -- certificate.
    acm :: Prelude.Maybe VirtualGatewayListenerTlsAcmCertificate,
    -- | A reference to an object that represents a virtual gateway\'s
    -- listener\'s Secret Discovery Service certificate.
    sds :: Prelude.Maybe VirtualGatewayListenerTlsSdsCertificate,
    -- | A reference to an object that represents a local file certificate.
    file :: Prelude.Maybe VirtualGatewayListenerTlsFileCertificate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListenerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acm', 'virtualGatewayListenerTlsCertificate_acm' - A reference to an object that represents an Certificate Manager
-- certificate.
--
-- 'sds', 'virtualGatewayListenerTlsCertificate_sds' - A reference to an object that represents a virtual gateway\'s
-- listener\'s Secret Discovery Service certificate.
--
-- 'file', 'virtualGatewayListenerTlsCertificate_file' - A reference to an object that represents a local file certificate.
newVirtualGatewayListenerTlsCertificate ::
  VirtualGatewayListenerTlsCertificate
newVirtualGatewayListenerTlsCertificate =
  VirtualGatewayListenerTlsCertificate'
    { acm =
        Prelude.Nothing,
      sds = Prelude.Nothing,
      file = Prelude.Nothing
    }

-- | A reference to an object that represents an Certificate Manager
-- certificate.
virtualGatewayListenerTlsCertificate_acm :: Lens.Lens' VirtualGatewayListenerTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsAcmCertificate)
virtualGatewayListenerTlsCertificate_acm = Lens.lens (\VirtualGatewayListenerTlsCertificate' {acm} -> acm) (\s@VirtualGatewayListenerTlsCertificate' {} a -> s {acm = a} :: VirtualGatewayListenerTlsCertificate)

-- | A reference to an object that represents a virtual gateway\'s
-- listener\'s Secret Discovery Service certificate.
virtualGatewayListenerTlsCertificate_sds :: Lens.Lens' VirtualGatewayListenerTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsSdsCertificate)
virtualGatewayListenerTlsCertificate_sds = Lens.lens (\VirtualGatewayListenerTlsCertificate' {sds} -> sds) (\s@VirtualGatewayListenerTlsCertificate' {} a -> s {sds = a} :: VirtualGatewayListenerTlsCertificate)

-- | A reference to an object that represents a local file certificate.
virtualGatewayListenerTlsCertificate_file :: Lens.Lens' VirtualGatewayListenerTlsCertificate (Prelude.Maybe VirtualGatewayListenerTlsFileCertificate)
virtualGatewayListenerTlsCertificate_file = Lens.lens (\VirtualGatewayListenerTlsCertificate' {file} -> file) (\s@VirtualGatewayListenerTlsCertificate' {} a -> s {file = a} :: VirtualGatewayListenerTlsCertificate)

instance
  Core.FromJSON
    VirtualGatewayListenerTlsCertificate
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayListenerTlsCertificate"
      ( \x ->
          VirtualGatewayListenerTlsCertificate'
            Prelude.<$> (x Core..:? "acm")
            Prelude.<*> (x Core..:? "sds")
            Prelude.<*> (x Core..:? "file")
      )

instance
  Prelude.Hashable
    VirtualGatewayListenerTlsCertificate
  where
  hashWithSalt
    salt'
    VirtualGatewayListenerTlsCertificate' {..} =
      salt' `Prelude.hashWithSalt` file
        `Prelude.hashWithSalt` sds
        `Prelude.hashWithSalt` acm

instance
  Prelude.NFData
    VirtualGatewayListenerTlsCertificate
  where
  rnf VirtualGatewayListenerTlsCertificate' {..} =
    Prelude.rnf acm `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf sds

instance
  Core.ToJSON
    VirtualGatewayListenerTlsCertificate
  where
  toJSON VirtualGatewayListenerTlsCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("acm" Core..=) Prelude.<$> acm,
            ("sds" Core..=) Prelude.<$> sds,
            ("file" Core..=) Prelude.<$> file
          ]
      )
