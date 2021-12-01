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
-- Module      : Amazonka.ELBV2.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an SSL server certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the certificate is the default certificate. Do not set
    -- this value when specifying a certificate as an input. This value is not
    -- included in the output when describing a listener, but is included when
    -- describing listener certificates.
    isDefault :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'certificate_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'isDefault', 'certificate_isDefault' - Indicates whether the certificate is the default certificate. Do not set
-- this value when specifying a certificate as an input. This value is not
-- included in the output when describing a listener, but is included when
-- describing listener certificates.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateArn = Prelude.Nothing,
      isDefault = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | Indicates whether the certificate is the default certificate. Do not set
-- this value when specifying a certificate as an input. This value is not
-- included in the output when describing a listener, but is included when
-- describing listener certificates.
certificate_isDefault :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Bool)
certificate_isDefault = Lens.lens (\Certificate' {isDefault} -> isDefault) (\s@Certificate' {} a -> s {isDefault = a} :: Certificate)

instance Core.FromXML Certificate where
  parseXML x =
    Certificate'
      Prelude.<$> (x Core..@? "CertificateArn")
      Prelude.<*> (x Core..@? "IsDefault")

instance Prelude.Hashable Certificate where
  hashWithSalt salt' Certificate' {..} =
    salt' `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData Certificate where
  rnf Certificate' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf isDefault

instance Core.ToQuery Certificate where
  toQuery Certificate' {..} =
    Prelude.mconcat
      [ "CertificateArn" Core.=: certificateArn,
        "IsDefault" Core.=: isDefault
      ]
