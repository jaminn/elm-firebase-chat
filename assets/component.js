customElements.define('float-div',
    class extends HTMLElement {
        constructor() {
            super();
            this._isTransitionActive = false;
        }

        connectedCallback() {
            this.exec();
            setInterval(() => { this.exec(); }, 100);
            //window.addEventListener('resize', () => { this.exec.call(this) });
        }

        get isTransitionActive() {
            return this._isTransitionActive;
        }

        set isTransitionActive(val) {
            this._isTransitionActive = val;
        }

        exec() {
            var ele = document.getElementById(this._targetEleId);
            if (ele) {
                var compStyles = window.getComputedStyle(ele);
                var rect = ele.getBoundingClientRect();
                this.style.position = "absolute";
                this.style.left = ele.offsetLeft + 'px';
                this.style.top = ele.offsetTop + 'px';
                this.style.width = rect.width + 'px';
                this.style.height = rect.height + 'px';
                this.style.fontSize = compStyles.getPropertyValue("font-size");
                this.style.borderRadius = compStyles.getPropertyValue("border-radius");
                if (this._isTransitionActive) {
                    this.style.transition = "left 300ms ease 0s, top 300ms ease 0s, width 300ms ease 0s, height 300ms ease 0s, border-radius 300ms ease 0s, font-size 300ms ease 0s, background-color 300ms ease 0s, opacity 300ms ease 0s";
                } else {
                    this.style.transition = "";
                }
            }
        }


        get targetEleId() {
            return this._targetEleId;
        }

        set targetEleId(val) {
            this._targetEleId = val;

            if (this.isConnected) {
                this.style.transition = "";
                this.exec();

                this.style.zIndex = 101;
                if(this.zIndexIntervalId) {
                    clearInterval(this.zIndexIntervalId);
                }
                this.zIndexIntervalId = setInterval(() => { this.style.zIndex = 100; }, 500);
            }
        }

    }
);